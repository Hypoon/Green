{-# LANGUAGE ForeignFunctionInterface #-}

module LowLevelGraphics
( initialize
, close
, clearDrawing
, updateDrawing
, drawHexagon
, drawDot
, drawText
) where

import Foreign.C
import RGB

foreign import ccall unsafe "graphics.c myinitialize" c_init :: CInt -> CInt -> IO()
initialize :: (Int,Int) -> IO()
initialize (w,h) = c_init (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "graphics.c myclose" c_close :: IO()
close :: IO()
close = c_close

foreign import ccall unsafe "graphics.c clearDrawing" c_clearDrawing :: IO()
clearDrawing :: IO()
clearDrawing = c_clearDrawing

foreign import ccall unsafe "graphics.c updateDrawing" c_updateDrawing :: IO()
updateDrawing :: IO()
updateDrawing = c_updateDrawing

foreign import ccall unsafe "graphics.c drawHexagon" c_drawHexagon :: CInt -> CInt -> CInt -> CInt -> CInt -> IO()
drawHexagon :: (Int,Int) -> RGB -> IO()
drawHexagon (x,y) (r,g,b) = c_drawHexagon (fromIntegral x) (fromIntegral y) (fromIntegral r) (fromIntegral g) (fromIntegral b)

foreign import ccall unsafe "graphics.c drawDot" c_drawDot :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO()
drawDot :: (Int,Int) -> Int -> RGB -> IO()
drawDot (x,y) rad (r,g,b) = c_drawDot (fromIntegral x) (fromIntegral y) (fromIntegral rad) (fromIntegral r) (fromIntegral g) (fromIntegral b)

foreign import ccall unsafe "graphics.c writeText" c_writeText:: CString -> CInt -> CInt -> CInt -> CInt -> CInt -> IO()
drawText :: String -> (Int,Int) -> RGB -> IO()
drawText str (x,y) (r,g,b) = do
    cstr <- (newCString str)
    c_writeText cstr (fromIntegral x) (fromIntegral y) (fromIntegral r) (fromIntegral g) (fromIntegral b)
