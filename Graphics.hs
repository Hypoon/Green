module Graphics
( screen_width
, screen_height
, renderDot
, renderHextille
, renderText
, displayAll
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

renderHextille :: Hextille -> (Hex,Double) -> Renderable
--renderHextille hextille (centerhex,rotation) = (drawOffsetHextille hextille (getXY centerhex) rotation)
renderHextille hextille (centerhex,rotation) = (drawOffsetHextille hextille (centerhex,rotation))

renderText :: String -> (Int,Int) -> RGB -> Renderable
renderText str (x,y) rgb = (drawText str (x,y) rgb)

displayAll :: Stack -> IO()
displayAll things = do
    clearDrawing
    sequence things
    updateDrawing

drawOffsetHex :: (Hex,Type) -> (Int,Int) -> Double -> IO()
drawOffsetHex (h,t) (ox,oy) angle = do
    drawHexagon (-ox+hx+(screen_width`quot`2),-oy+hy+(screen_height`quot`2)) angle (typeToRGB t) where
        (hx,hy) = (getXY h)

--drawOffsetHextille :: Hextille -> (Int,Int) -> Double -> IO()
drawOffsetHextille :: Hextille -> (Hex, Double) -> IO()
--drawOffsetHextille hextille (ox, oy) rotation = do
drawOffsetHextille hextille (centerHex,rotation) = do
    sequence $ map (\(h,t) -> drawOffsetHex ((rotateHex h centerHex rotation),t) (getXY centerHex) (rotation*pi*(1/3))) hextille
    return ()
