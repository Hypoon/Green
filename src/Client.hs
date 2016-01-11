import System.IO
import Logger
import Control.Concurrent
import Control.Concurrent.Chan
import Data.List
import Services

main = do
    putStrLn "Client"
    tidc <- newChan
    c <- initLogger "testLog" tidc

    -- test code --
    runLogIO c someLoggingFunction
    runLogIO c $ qlog "!!quit!!"
    -- end test code --

    waitLoop tidc [Logger]

someLoggingFunction :: LogIO()
someLoggingFunction = do
    qlog "test1"
    qlog "test2"

waitLoop :: ServiceChan -> [Service] -> IO ()
waitLoop tidc [] = return ()
waitLoop tidc list = do
    quitter <- readChan tidc
    waitLoop tidc (delete quitter list)
