import System.IO
import Logger
import Control.Concurrent
import Control.Concurrent.Chan
import Data.List
import Services

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
