{-# LANGUAGE RankNTypes #-}

import System.IO
import Network
import Control.Monad
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

host :: (forall t m. StateServer t m) -> Handle -> IO ()
host myGuest net = do
    runSpiderHost $ do
        (e, eTriggerRef) <- newEventWithTriggerRef
        b <- runHostFrame $ myGuest e
        forever $ do
            input <- liftIO (liftM read $ hGetLine net)
            mETrigger <- liftIO $ readIORef eTriggerRef
            case mETrigger of
                Nothing -> do
                    return ()
                Just eTrigger -> do
                    fireEvents [eTrigger :=> input]
            output <- runHostFrame $ sample b
            liftIO $ hPutStrLn net $ show output

guest :: StateServer t m
guest e = do
    --f <- holdDyn (MoveCommand ' ') e
    --d <- mapDyn (processInput) f
    d <- foldDyn (\a b -> addNewLoc (processInput a) b) (NewLocation (0,0,0)) e
    return $ current d

main = do
    putStrLn "State Server"
    ssock <- listenOn (PortNumber 65008)
    (net,_,_) <- accept ssock
    host guest net
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
