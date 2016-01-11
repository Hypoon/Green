module Services
( ServiceChan (..)
, Service(..)
) where

import Control.Concurrent
import Control.Concurrent.Chan
import Data.Time

data Service = Logger | Poller | Connection Int deriving (Eq)
type ServiceChan = Chan Service
data SomethingChan = LogChan (Chan (UTCTime,String))
