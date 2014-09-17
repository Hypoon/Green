module Graphics
( screen_width
, screen_height
--, getXY
--, drawOffsetHextille
--, drawText
, renderDot
, renderHextille
--, renderSlidingHextille
, renderText
, displayAll
--, animateMotion
--, showEverything
, startGraphics
, stopGraphics
, Stack(..)
, Renderable(..)
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
getXY (Hex(u,v,w)) = (round $ (-2*u)*75,round $ -(v-w)*50)

type Renderable = IO()
type Stack = [Renderable]

renderDot :: Renderable
renderDot = (drawDot (screen_width `quot` 2,screen_height `quot` 2) 25 (0,0x80,0xFF))

renderHextille :: Hextille -> Hex -> Renderable
renderHextille hextille centerhex = (drawOffsetHextille hextille (getXY centerhex))

{-
renderSlidingHextille :: Hextille -> Hex -> Hex -> Time -> Renderable
renderSlidingHextille hextille old new startTime = (\t -> drawHextileMotionFrame hextille old new (t-startTime))
-}

renderText :: String -> (Int,Int) -> RGB -> Renderable
renderText str (x,y) rgb = (drawText str (x,y) rgb)

displayAll :: Stack -> IO()
displayAll things = do
    clearDrawing
    sequence things
    updateDrawing

drawOffsetHex :: (Hex,Type) -> (Int,Int) -> IO()
drawOffsetHex (h,t) (ox,oy) = do
    drawHexagon (-ox+hx+(screen_width`quot`2),-oy+hy+(screen_height`quot`2)) (typeToRGB t) where
        (hx,hy) = (getXY h)

drawOffsetHextille :: Hextille -> (Int,Int) -> IO()
drawOffsetHextille hextille (ox, oy) = do
    sequence $ map (\(h,t) -> drawOffsetHex (h,t) (ox, oy) ) hextille
    return ()

{-
drawHextileMotionFrame :: Hextille -> Hex -> Hex -> Time -> IO()
drawHextileMotionFrame hextille old new time = do
    let (oldx,oldy) = (getXY old)
        (newx,newy) = (getXY new)
        prog = (fromIntegral time)/500.0::Float
        x=oldx+(round (prog*(fromIntegral (newx-oldx))))
        y=oldy+(round (prog*(fromIntegral (newy-oldy))))
    drawOffsetHextille hextille (x,y)
-}
{-
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
        clearDrawing
        drawHextileMotionFrame ht old (old `addHex` dir) (nowTick - startTick)
        drawDot (screen_width `quot` 2,screen_height `quot` 2) 25 (0,0x80,0xFF)
        updateDrawing
        newNowTick <- getTicks
        animateMotion ht old startTick newNowTick dir

showEverything :: Hextille -> Hex -> IO()
showEverything hextille centerhex = do
    clearDrawing
    drawOffsetHextille hextille (getXY centerhex)
    drawDot (screen_width `quot` 2,screen_height `quot` 2) 25 (0,0x80,0xFF)
    drawText (show (smallLoc centerhex)) (screen_width,screen_height) (0,0,0)
    updateDrawing
-}
