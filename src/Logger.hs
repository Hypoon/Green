module Logger
( qlog
) where

import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
--import Control.Monad.Reader
import Data.Time
import Services

type LogChan = Chan (UTCTime,String)

data Logger = Logger LogChan

instance Service Logger where
    start path = initLogger path
    stop (Logger _) = qlog "!!quit!!"

askLogger :: ServiceIO(Logger)
askLogger = do
    services <- ask
    logger = head $ filter isLogger services
    return logger

isLogger :: Service -> Bool
isLogger (Logger _ _) = True
isLogger _ = False

initLogger :: String -> ServiceIO(Logger)
initLogger path = do
    logChan <- newChan
    logFile <- openFile path AppendMode
    let logger = Logger logChan
    serviceForkIO (startLoggerLoop path)
    return (Logger logChan)

qlog :: String -> ServiceIO()
qlog message = do
    logChan <- askLogger
    time <- liftIO $ getCurrentTime
    liftIO $ writeChan logChan (time,message)

startLoggerLoop :: String -> ServiceIO()
startLoggerLoop path = do
    (ServiceManager schan) <- askSM
    logger <- askLogger
    liftIO $ hPutStrLn logFile ((showT time)++": "++"Starting Logger.")
    logFile <- liftIO $ openFile path AppendMode
    liftIO $ writeChan schan (ServiceStarted logger)
    loggerLoop logFile
    liftIO $ hPutStrLn logFile ((showT time)++": "++"Stopping Logger.")
    liftIO $ hClose logFile
    liftIO $ writeChan schan (ServiceStopped logger)
    return()

loggerLoop :: Handle -> ServiceIO()
loggerLoop logFile = do
    (Logger logChan) <- askLogger
    (ServiceManager schan) <- askSM
    (time,message) <- liftIO $ readChan logChan
    if(message=="!!quit!!")
        then do
            return ()
        else do
            liftIO $ hPutStrLn logFile ((showT time)++": "++message)
            liftIO $ hFlush logFile
            loggerLoop logFile tidc

showT :: UTCTime -> String
showT time = let [a,b,c] = (words (show time))
                 in unwords [a,b++(replicate (15 - (length b)) '0'),c]
