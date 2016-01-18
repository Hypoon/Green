module Services
( ServiceChan (..)
, Service
, ServiceMsg(..)
, ServiceIO(..)
, runServiceIO
, serviceForkIO
) where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Time
import Control.Monad.Reader
import System.IO

type ServiceIO = ReaderT [a] IO

class Service a where
    start :: b -> ServiceIO(a)
    stop  :: a -> ServiceIO()

data ServiceMsg = ServiceStarted Service | ServiceStopped Service
type ServiceChan = Chan ServiceMsg




data ServiceManager = ServiceManager ServiceChan

instance Service ServiceManager where
    start () = initServiceManager
    stop (ServiceManager _) = return ()

initServiceManager :: ServiceIO (Service)
initServiceManager = do
    schan <- liftIO $ newChan
    return (ServiceManager schan)

askSM :: ServiceIO(ServiceManager)
askSM = do
    services <- ask
    let sm = head $ filter isServiceManager services
    return sm

isServiceManager :: Service -> Bool
isServiceManager (ServiceManager _) = True
isServiceManager _ = False






runServiceIO :: [Service] -> ServiceIO a -> IO a
runServiceIO ss sio = runReaderT sio ss

serviceForkIO :: ServiceIO() -> ServiceIO()
serviceForkIO sio = do
    services <- ask
    liftIO $ forkIO(runServiceIO services sio)
