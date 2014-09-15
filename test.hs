{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Hex
import Graphics
{-
screen_height=750
screen_width=1200

foreign import ccall unsafe "graphics.c myinitialize" c_init :: CInt -> CInt -> IO()
initialize :: (Int,Int) -> IO()
initialize (w,h) = c_init (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe "graphics.c myclose" c_close :: IO()
foreign import ccall unsafe "graphics.c clearDrawing" c_clearDrawing :: IO()
foreign import ccall unsafe "graphics.c updateDrawing" c_updateDrawing :: IO()

foreign import ccall unsafe "graphics.c drawHexagon" c_drawHexagon :: CInt -> CInt -> CInt -> CInt -> CInt -> IO()
drawHexagon :: (Int,Int) -> (Int,Int,Int) -> IO()
drawHexagon (x,y) (r,g,b) = c_drawHexagon (fromIntegral x) (fromIntegral y) (fromIntegral r) (fromIntegral g) (fromIntegral b)

foreign import ccall unsafe "graphics.c getEvent" c_getEvent :: IO(CChar)
getKey :: IO(Char)
getKey = do
    cletter <- c_getEvent
    return (castCCharToChar cletter)

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
getXY (Hex(u,v,w)) = (-2*u,-(v-w))

charToHex :: Char -> Hex
charToHex 'w' = (Hex(0,1,-1))
charToHex 'q' = (Hex(1,0,-1))
charToHex 'e' = (Hex(-1,1,0))
charToHex 's' = (Hex(0,-1,1))
charToHex 'd' = (Hex(-1,0,1))
charToHex 'a' = (Hex(1,-1,0))
charToHex ' ' = (Hex(0,0,0))

animateMotion :: Hextille -> Hex -> Float -> Hex -> IO()
animateMotion ht _ 1.0 _ = return ()
animateMotion ht old prog dir 
    | prog>1.0  = return()
    | otherwise = do
        c_clearDrawing
        sequence $ map (\(h,t) -> drawHexagon ((\(x,y)->(floor $ x*75+((fromIntegral screen_width)/2.0),floor $ y*50+((fromIntegral screen_height)/2.0)))((\(x,y) -> ((fromIntegral x)-(prog*(fromIntegral(fst (getXY dir)))), (fromIntegral y)-(prog*(fromIntegral (snd (getXY dir))))) )(getXY (subHex h old)))) (typeToRGB t)) ht
        c_updateDrawing
        getLine
        animateMotion ht old (prog+0.04) dir
-}
{-
debug :: Hextille -> IO()
debug rh = do
    clearDrawing
    let old = (Hex(0,0,0))
        dir = (Hex(-1,0,1))
        prog = 0.22
    sequence ( map (\(h,t) -> drawHexagon ((\(x,y)->(floor $ x*75+((fromIntegral screen_width)/2.0),floor $ y*50+((fromIntegral screen_height)/2.0)))((\(x,y) -> ((fromIntegral x)-(prog*(fromIntegral(fst (getXY dir)))), (fromIntegral y)-(prog*(fromIntegral (snd (getXY dir))))) )(getXY (subHex h old)))) (typeToRGB t)) rh )
    updateDrawing
    getLine
    return ()
-}
{-
showEverything :: Hextille -> Hex -> IO()
showEverything hextille centerhex = do
    key <- getKey
    let newCenterhex = centerhex `addHex` (charToHex key)
    if (key/=' ')
        then animateMotion hextille centerhex 0.0 (charToHex key)
        else (return())
    clearDrawing
    sequence $ map (\(h,t) -> drawHexagon ((\(x,y)->(x*75+(screen_width`quot`2),y*50+(screen_height`quot`2)))(getXY (subHex h newCenterhex))) (typeToRGB t)) hextille
    updateDrawing
    showEverything hextille newCenterhex
-}
main = do
    initialize (screen_width,screen_height)
    let rh = randomHextille 0 3
    --c_clearDrawing
    --sequence $ map (\(h,t) -> drawHexagon ((\(x,y)->(x*75+(screen_width`quot`2),y*50+(screen_height`quot`2)))(getXY h)) (typeToRGB t)) rh
    --debug rh
    --c_updateDrawing
    --getLine
    --showEverything rh (Hex(0,0,0))
    run rh (Hex(0,0,0))
    close
