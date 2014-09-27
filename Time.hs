{-# LANGUAGE ForeignFunctionInterface #-}

module Time
( getTicks
, sleep
, Time(..)
) where

import Foreign.C
import Data.Word

type Time = Word32

foreign import ccall unsafe "SDL.h SDL_GetTicks" c_getTicks :: IO(CUInt)
getTicks :: IO(Time)
getTicks = do
    ticks <- c_getTicks
    return (fromIntegral ticks)

foreign import ccall unsafe "time.c sleepUntilNextFrame" c_sleep :: IO()
sleep :: IO()
sleep = c_sleep
