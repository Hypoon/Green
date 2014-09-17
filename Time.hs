{-# LANGUAGE ForeignFunctionInterface #-}

module Time
( getTicks
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
