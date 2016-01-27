{-# LANGUAGE RankNTypes #-}

import System.IO
import Network
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.Async
import Reflex
import Control.Monad
import Data.IORef (readIORef)
import Reflex.Host.Class (newEventWithTriggerRef, runHostFrame, fireEvents)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity (Identity(..))
import Data.Dependent.Sum (DSum ((:=>)))
import Control.Monad.Fix (MonadFix)

data MoveCommand = MoveCommand Char deriving (Read,Show)
data NewLocation = NewLocation (Int,Int,Int) deriving (Read,Show)

charToDir :: Char -> (Int,Int,Int)
charToDir 'w' = (0,1,-1)
charToDir 's' = (0,-1,1)
charToDir 'q' = (1,0,-1)
charToDir 'e' = (-1,1,0)
charToDir 'a' = (1,-1,0)
charToDir 'd' = (-1,0,1)
charToDir _ = (0,0,0)

addLoc :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
addLoc (a,b,c) (d,e,f) = (a+d,b+e,c+f)

addNewLoc :: NewLocation -> NewLocation -> NewLocation
addNewLoc (NewLocation a) (NewLocation b) = NewLocation (a `addLoc` b)

processInput :: MoveCommand -> NewLocation
processInput (MoveCommand c) = NewLocation (charToDir c)

type StateServer t m = (Reflex t, MonadHold t m, MonadFix m) => Event t MoveCommand -> m (Behavior t NewLocation)

host :: (forall t m. StateServer t m) -> Chan MoveCommand -> Chan NewLocation -> IO ()
host myGuest inputChan outputChan = do
    runSpiderHost $ do
        (e, eTriggerRef) <- newEventWithTriggerRef
        b <- runHostFrame $ myGuest e
        forever $ do
            input <- liftIO (readChan inputChan)
            mETrigger <- liftIO $ readIORef eTriggerRef
            case mETrigger of
                Nothing -> do
                    return ()
                Just eTrigger -> do
                    fireEvents [eTrigger :=> input]
            output <- runHostFrame $ sample b
            liftIO $ writeChan outputChan output

guest :: StateServer t m
guest e = do
    --f <- holdDyn (MoveCommand ' ') e
    --d <- mapDyn (processInput) f
    d <- foldDyn (\a b -> addNewLoc (processInput a) b) (NewLocation (0,0,0)) e
    return $ current d

-- host guest inputChan


main = do
    putStrLn "State Server"
    ssock <- listenOn (PortNumber 65008)
    (net,_,_) <- accept ssock
    inputChan <- newChan
    outputChan <- newChan
    async (sequence $ repeat $ (hGetLine net >>= (writeChan inputChan . read)))
    async (sequence $ repeat $ (readChan outputChan >>= (hPutStrLn net . show)))
    host guest inputChan outputChan
--    let loop loc = do
--            incoming <- hGetLine net
--            let (MoveCommand c) = read incoming
--            if (c /= '\x1b')
--              then do
--                let newLoc = loc `addLoc` (charToDir c)
--                hPutStrLn net $ show $ NewLocation newLoc
--                loop newLoc
--              else do
--                hClose net
--                sClose ssock
--    loop (0,0,0)
--    return ()
