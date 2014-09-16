{-# LANGUAGE ForeignFunctionInterface #-}

module Graphics
( screen_height
, screen_width
, initialize
, close
, clearDrawing
, updateDrawing
, drawHexagon
, getKey
, lighten
, darken
, typeToRGB
, getXY
, charToHex
, animateMotion
, showEverything
, run
) where

import Foreign.C
import Hex
import Data.Word

screen_height=750
screen_width=1200

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
drawHexagon :: (Int,Int) -> (Int,Int,Int) -> IO()
drawHexagon (x,y) (r,g,b) = c_drawHexagon (fromIntegral x) (fromIntegral y) (fromIntegral r) (fromIntegral g) (fromIntegral b)

foreign import ccall unsafe "graphics.c drawDot" c_drawDot :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO()
drawDot :: (Int,Int) -> Int -> (Int,Int,Int) -> IO()
drawDot (x,y) rad (r,g,b) = c_drawDot (fromIntegral x) (fromIntegral y) (fromIntegral rad) (fromIntegral r) (fromIntegral g) (fromIntegral b)

foreign import ccall unsafe "graphics.c getEvent" c_getEvent :: IO(CChar)
getKey :: IO(Char)
getKey = do
    cletter <- c_getEvent
    return (castCCharToChar cletter)

foreign import ccall unsafe "SDL.h SDL_GetTicks" c_getTicks :: IO(CUInt)
getTicks :: IO(Word32)
getTicks = do
    ticks <- c_getTicks
    return (fromIntegral ticks)

foreign import ccall unsafe "graphics.c writeText" c_writeText:: CString -> CInt -> CInt -> CInt -> CInt -> CInt -> IO()
writeText :: String -> (Int,Int) -> (Int,Int,Int) -> IO()
writeText str (x,y) (r,g,b) = do
    cstr <- (newCString str)
    c_writeText cstr (fromIntegral x) (fromIntegral y) (fromIntegral r) (fromIntegral g) (fromIntegral b)

lighten :: (Int,Int,Int) -> (Int,Int,Int)
lighten (r,g,b) = ((0xFF+r) `quot` 2,(0xFF+g) `quot` 2,(0xFF+b) `quot` 2)

darken :: (Int,Int,Int) -> (Int,Int,Int)
darken (r,g,b) = (r `quot` 2, g `quot` 2, b `quot` 2)

typeToRGB :: Type -> (Int,Int,Int)
typeToRGB (Type (Light,c)) = lighten (typeToRGB (Type (Shadeless,c)))
typeToRGB (Type (Dark,c)) = darken (typeToRGB (Type (Shadeless,c)))
typeToRGB (Type (Shadeless,Red)) = (0xFF,0,0)
typeToRGB (Type (Shadeless,Green)) = (0,0xFF,0)
typeToRGB (Type (Shadeless,Blue)) = (0,0,0xFF)
typeToRGB (Type (Shadeless,Colorless)) = (0x80,0x80,0x80)

getXY :: Hex -> (Int,Int)
getXY (Hex(u,v,w)) = ((-2*u)*75,-(v-w)*50)

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

processChar :: Char -> Hextille -> Hex -> IO()
processChar '\ESC' _ _ = return()
processChar ' ' hextille centerhex = showEverything hextille centerhex
processChar c hextille centerhex = do
    nowTick <- getTicks
    animateMotion hextille centerhex nowTick nowTick (charToHex c)

displayOffsetHex :: (Hex,Type) -> (Int,Int) -> IO()
displayOffsetHex (h,t) (ox,oy) = do
    drawHexagon (-ox+hx+(screen_width`quot`2),-oy+hy+(screen_height`quot`2)) (typeToRGB t) where
        (hx,hy) = (getXY h)

displayHextille :: Hextille -> (Int,Int) -> IO()
displayHextille hextille (ox,oy) = do
    clearDrawing
    sequence $ map (\(h,t) -> displayOffsetHex (h,t) (ox, oy) ) hextille
    drawDot (600,375) 25 (0,0x80,0xFF)
    updateDrawing

displayText :: String -> (Int,Int) -> (Int,Int,Int) -> IO()
displayText a b c = do
    writeText a b c
    updateDrawing

animateMotion :: Hextille -> Hex -> Word32 -> Word32 -> Hex -> IO()
animateMotion ht old startTick nowTick dir 
    | nowTick>=startTick+1000  = run ht (old `addHex` dir)
    | otherwise = do
        let new = old `addHex` dir
            (oldx,oldy) = (getXY old)
            (newx,newy) = (getXY new)
            prog = ((fromIntegral nowTick)-(fromIntegral startTick))/1000.0::Float
            x=oldx+(round (prog*(fromIntegral (newx-oldx))))
            y=oldy+(round (prog*(fromIntegral (newy-oldy))))
        displayHextille ht (x,y)
        newNowTick <- getTicks
        animateMotion ht old startTick newNowTick dir

showEverything :: Hextille -> Hex -> IO()
showEverything hextille centerhex = do
    displayHextille hextille (getXY centerhex)
    displayText (show (smallLoc centerhex)) (1200,750) (0,0,0)
    run hextille centerhex

run :: Hextille -> Hex -> IO()
run hextille centerhex = do
    key <- getKey
    processChar key hextille centerhex
