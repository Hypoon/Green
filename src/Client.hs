import System.IO
--import Logger
import Control.Concurrent
import Control.Concurrent.Chan
import Data.List
--import Services
import Control.Monad
import System.Environment
import Network

{-
main = do
    putStrLn "Client"
    (sm::ServiceManager) <- runServiceIO [] (start ())
    (logger::Logger) <- runServiceIO [sm] (start "testLog")
    -- test code --
    runServiceIO [sm,logger] someLoggingFunction
    runServiceIO [sm,logger] (stop logger)
    -- end test code --
    waitLoop

svcmain :: ServiceIO ()
svcmain = do
    (sm::ServiceManager) <- start ()
    local (sm:) do
        (logger::Logger) <- start "testLog"
        local (logger:) do
            someLoggingFunction
            stop logger
    waitLoop


someLoggingFunction :: ServiceIO()
someLoggingFunction = do
    qlog "test1"
    qlog "test2"

waitLoop :: ServiceIO ()
waitLoop = do
    services <- ask
    (serviceManager schan) <- askSM
    if (length services > 1)
        then
            message <- readChan schan
            local (waitLoopMessageHandler (ServiceStopped svc)) waitLoop
        else
            return ()

waitLoopMessageHandler :: ServiceMsg -> [Service] -> [Service]
waitLoopMessageHandler (ServiceStarted svc) list = (svc:list)
waitLoopMessageHandler (ServiceStopped svc) list = (delete svc list)
-}

getXY :: (Int,Int,Int) -> (Int,Int)
getXY (u,v,w) = (40+((-2*u)*2),12+(-(v-w)*1))

hexList :: [(Int,Int)]
hexList = sortOn (snd) $ map getXY $ allList

onScreen :: (Int,Int,Int) -> Bool
onScreen h = ((fst (getXY h)) >= 3) && ((fst (getXY h)) <= 78) && ((snd (getXY h)) >= 3) && ((snd (getXY h)) <= 23)

allList :: [(Int,Int,Int)]
allList = filter (onScreen) $ filter (\(u,v,w) -> (u+v+w)==0) [(u,v,w) | u <- [-4..4] , v <- [-4..4], w <- [-4..4]]

drawHexXY :: (Int,Int) -> IO()
drawHexXY (x,y) = do
    drawStr (x-1,y-1)   "___"
    drawStr (x-2,y)    "/   \\"
    drawStr (x-2,y+1) "\\___/"

drawStr :: (Int,Int) -> String -> IO()
drawStr (x,y) str = do
    putStr ("\x1b["++(show y)++";"++(show x)++"H"++str)

drawChar :: (Int,Int) -> Char -> IO()
drawChar (x,y) c = do
    drawStr (x,y) (c:[])

drawVLine :: Int -> (Int,Int) -> IO()
drawVLine x (a,b) = do
    mapM_ (\h -> drawChar (x,h) '|') [a..b]

drawHLine :: Int -> (Int,Int) -> IO()
drawHLine y (a,b) = do
    drawStr (a,y) (replicate (1+b-a) '-')

drawBox :: (Int,Int) -> IO()
drawBox (width,height) = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr "\x1b[2J"
    putStr "\x1b[?25l"
    drawChar (1,1) '+'
    drawChar (1,height) '+'
    drawChar (width,1) '+'
    drawChar (width,height) '+'
    drawVLine 1 (2,height-1)
    drawVLine width (2,height-1)
    drawHLine 1 (2,width-1)
    drawHLine height (2,width-1)
    return ()

addToolBox :: Int -> IO()
addToolBox num = do
    let w = 5
    let h = 3
    drawChar (80,23) '+'
    let drawPart n = do
            drawChar (80-((w-1)*n),25-(h-1)) '+'
            drawChar (80-((w-1)*n),25) '+'
            drawVLine (80-((w-1)*n)) (25-(h-1)+1,25-1)
            drawHLine (25-(h-1)) (80-(((w-1)*n)-1),80-((w-1)*(n-1)+1))
            drawChar (80-((w-1)*n)+((w-1) `quot` 2),25-((h-1) `quot` 2)) (head $ show (num-(n-1)))
    mapM_ drawPart [1..num]


healthWidth = 20
addHealthBox :: IO()
addHealthBox = do
    drawChar (80-(healthWidth-1),1) '+'
    drawChar (80-(healthWidth-1),3) '+'
    drawChar (80,3) '+'
    drawChar (80-(healthWidth-1),2) '|'
    drawHLine 3 (80-(healthWidth-1)+1,80-1)

drawHealth :: Double -> IO()
drawHealth f = do
    let num = ceiling $ f * (realToFrac $ healthWidth-2)
    let str = (replicate num '#')++(replicate ((healthWidth-2)-num) ' ')
    drawStr (80-(healthWidth-1)+1,2) str

data MoveCommand = MoveCommand Char deriving (Read,Show)
data NewLocation = NewLocation (Int,Int,Int) deriving (Read,Show)

main = do
    args <- getArgs
    let hostname = if (length args == 0)
        then "127.0.0.1"
        else (head args)
    putStrLn "Woo"
    drawBox (80,25)
    addHealthBox
    drawHealth 0.3
    addToolBox 3
    mapM_ drawHexXY hexList
    {-let l1 = (map (\h -> drawChar (getXY h) 'U') allList)
    let l2 = (map (\h -> drawChar (getXY h) ' ') allList)
    let l3 = repeat (threadDelay 500000)
    sequence_ $ concat $ (zipWith3 (\a b c -> [a,b,c]) l1 l3 l2) -}
    net <- connectTo hostname (PortNumber 65008)
    let loop loc = do
          drawChar (getXY loc) 'U' 
          dir <- getChar
          if (dir /= '\x1b')
            then do
              hPutStrLn net (show $ MoveCommand dir)
              resp <- hGetLine net
              let (NewLocation newLoc) = read resp
              drawChar (getXY loc) ' '
              loop newLoc
            else do
              hPutStrLn net (show $ MoveCommand dir)
              hClose net
    loop (0,0,0)
    putStr ("\x1b[25;80H")
    putStr "\x1b[?25h"
    putStrLn ""
