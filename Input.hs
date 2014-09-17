{-# LANGUAGE ForeignFunctionInterface #-}

module Input
( getKey
, charToHex
) where

import Foreign.C
import Hex

foreign import ccall unsafe "time.c getEvent" c_getEvent :: IO(CChar)
getKey :: IO(Char)
getKey = do
    cletter <- c_getEvent
    return (castCCharToChar cletter)

charToHex :: Char -> Hex
charToHex 'w' = (Hex(0,1,-1))
charToHex 'q' = (Hex(1,0,-1))
charToHex 'e' = (Hex(-1,1,0))
charToHex 's' = (Hex(0,-1,1))
charToHex 'd' = (Hex(-1,0,1))
charToHex 'a' = (Hex(1,-1,0))
charToHex 'p' = (Hex(0,1,-1))
charToHex 'o' = (Hex(1,0,-1))
charToHex '[' = (Hex(-1,1,0))
charToHex ';' = (Hex(0,-1,1))
charToHex '\'' = (Hex(-1,0,1))
charToHex 'l' = (Hex(1,-1,0))
charToHex _ = (Hex(0,0,0))
