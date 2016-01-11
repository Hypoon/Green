module Logger
( initLogger
, qlog
, runLogIO
, LogIO(..)
) where

import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Reader
import Data.Time
import Services

type LogIO = ReaderT LogChan IO
type LogChan = Chan (UTCTime,String)

initLogger :: String -> ServiceChan -> IO(LogChan)
initLogger path tidc = do
    logChan <- newChan
    logFile <- openFile path AppendMode
    runLogIO logChan (qlog "Starting Logger.")
    forkIO (runLogIO logChan (loggerLoop logFile tidc))
    return logChan

runLogIO :: LogChan -> LogIO a -> IO a
runLogIO c l = runReaderT l c

qlog :: String -> LogIO()
qlog message = do
    logChan <- ask
    time <- liftIO $ getCurrentTime
    liftIO $ writeChan logChan (time,message)

loggerLoop :: Handle -> ServiceChan -> LogIO()
loggerLoop logFile tidc = do
    logChan <- ask
    (time,message) <- liftIO $ readChan logChan
    if(message=="!!quit!!")
        then do
            liftIO $ hPutStrLn logFile ((showT time)++": "++"Stopping Logger.")
            liftIO $ hClose logFile
            liftIO $ writeChan tidc Logger
            return ()
        else do
            liftIO $ hPutStrLn logFile ((showT time)++": "++message)
            liftIO $ hFlush logFile
            loggerLoop logFile tidc

showT :: UTCTime -> String
showT time = let [a,b,c] = (words (show time))
                 in unwords [a,b++(replicate (15 - (length b)) '0'),c]
