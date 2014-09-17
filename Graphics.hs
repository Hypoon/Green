module Graphics
( getXY
, displayHextille
, drawText
, animateMotion
, showEverything
, startGraphics
, stopGraphics
) where

import Hex
import RGB
import LowLevelGraphics
import Time

screen_height=750::Int
screen_width=1200::Int

startGraphics :: IO()
startGraphics = initialize (screen_width,screen_height)

stopGraphics :: IO()
stopGraphics = close

getXY :: Hex -> (Int,Int)
getXY (Hex(u,v,w)) = ((-2*u)*75,-(v-w)*50)

type Renderable = (Int -> IO())

drawOffsetHex :: (Hex,Type) -> (Int,Int) -> IO()
drawOffsetHex (h,t) (ox,oy) = do
    drawHexagon (-ox+hx+(screen_width`quot`2),-oy+hy+(screen_height`quot`2)) (typeToRGB t) where
        (hx,hy) = (getXY h)

drawOffsetHextille :: Hextille -> (Int,Int) -> IO()
drawOffsetHextille hextille (ox, oy) = do
    sequence $ map (\(h,t) -> drawOffsetHex (h,t) (ox, oy) ) hextille
    return ()

displayHextille :: Hextille -> (Int,Int) -> IO()
displayHextille hextille (ox,oy) = do
    clearDrawing
    drawOffsetHextille hextille (ox,oy)
    drawDot (screen_width `quot` 2,screen_height `quot` 2) 25 (0,0x80,0xFF)
    updateDrawing

displayText :: String -> (Int,Int) -> RGB -> IO()
displayText a b c = do
    drawText a b c
    updateDrawing

animateMotion :: Hextille -> Hex -> Time -> Time -> Hex -> IO()
animateMotion ht old startTick nowTick dir 
    | nowTick>=startTick+500  = return()
    | otherwise = do
        let new = old `addHex` dir
            (oldx,oldy) = (getXY old)
            (newx,newy) = (getXY new)
            prog = ((fromIntegral nowTick)-(fromIntegral startTick))/500.0::Float
            x=oldx+(round (prog*(fromIntegral (newx-oldx))))
            y=oldy+(round (prog*(fromIntegral (newy-oldy))))
        displayHextille ht (x,y)
        newNowTick <- getTicks
        animateMotion ht old startTick newNowTick dir

showEverything :: Hextille -> Hex -> IO()
showEverything hextille centerhex = do
    displayHextille hextille (getXY centerhex)
    displayText (show (smallLoc centerhex)) (screen_width,screen_height) (0,0,0)
