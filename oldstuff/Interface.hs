module Interface
( startInterface
, VisualStack(..)
) where

import Hex
import Graphics
import Input
import Time

import Numeric

data Status = Quitting | Moving Hex Hex Time | Idle | Rotating Double Double Time
data Action = DoQuit | DoIdle | DoMove Hex | DoRotate Double
data Visual = DebugText | Dot | Ground Hextille
type VisualStack = [Visual]
type Frame = (Hex,Double)


hexToText :: Hex -> String
hexToText hex = ("("++(showFFloat (Just 1) u $ ","++(showFFloat (Just 1) v $ ","++(showFFloat (Just 1) w ")")))) where (u,v,w) = (smallLoc hex)

debugText :: Frame -> String
debugText (h,r) = hexToText h ++ " " ++ "R" ++ (showFFloat (Just 1) r "")

renderVisual :: Visual -> Frame -> Time -> Renderable
renderVisual Dot _ _ = renderDot
renderVisual DebugText frame _ = (renderText (debugText frame) (screen_width,screen_height) (0,0,0))
renderVisual (Ground hextille) frame _ = (renderHextille hextille frame)

renderAll :: VisualStack -> Frame -> Time -> IO()
renderAll vstack frame time = do
    let stack = map (\visual -> renderVisual visual frame time) vstack
    displayAll stack

processChar :: Char -> Action
processChar '\ESC' = DoQuit
processChar ' ' = DoIdle
processChar 'r' = DoRotate 1
processChar 'f' = DoRotate (-1)
processChar char = DoMove (charToHex char)

actionToStatus :: Action -> Frame -> Time -> Status
actionToStatus DoQuit _ _ = Quitting
actionToStatus DoIdle _ _ = Idle
actionToStatus (DoMove dir) (oldHex,rotation) now = Moving oldHex (oldHex `addHex` (roundHex (rotateHexAboutOrigin dir (-rotation)))) now
actionToStatus (DoRotate angle) (axis,oldrotation) now
    | (oldrotation + angle < -0.5) = Rotating (oldrotation+6.0) (oldrotation + angle + 6.0) now
    | otherwise = Rotating oldrotation (oldrotation + angle) now

run :: (Status,Frame,VisualStack) -> IO()
run (Idle,frame,vstack) = do
    now <- getTicks
    renderAll vstack frame now
    key <- getKey
    let action = (processChar key)
    let newStatus = actionToStatus action frame now
    sleep
    run (newStatus,frame,vstack)
run (Quitting,_,_) = return()
run (Moving src dest startTime,(centerHex,rotation),vstack) = do
    now <- getTicks
    let interval = now - startTime
    if (interval < 500)
        then do
            let newCenterHex = (src `addHex` ( ((fromIntegral interval)/500.0) `scaleHex` (dest `subHex` src) ))
            renderAll vstack (newCenterHex,rotation) now
            sleep
            run (Moving src dest startTime,(newCenterHex,rotation),vstack)
        else do
            let newCenterHex = dest
            renderAll vstack (newCenterHex,rotation) now
            sleep
            run (Idle,(newCenterHex,rotation),vstack)
run (Rotating src dest startTime,(centerHex,rotation),vstack) = do
    now <- getTicks
    let interval = now - startTime
    if (interval < 500)
        then do
            let newRotation = (src + (((fromIntegral interval)/500.0)*(dest-src)))
            renderAll vstack (centerHex,newRotation) now
            sleep
            run (Rotating src dest startTime,(centerHex,newRotation),vstack)
        else do
            let newRotation = fromIntegral $ round dest `mod` 6
            renderAll vstack (centerHex,newRotation) now
            sleep
            run (Idle,(centerHex,newRotation),vstack)

startInterface :: Hextille -> IO()
startInterface rh = do
    startGraphics
    let vstack =    ( Ground rh
                    : Dot
                    : DebugText
                    : [])
    run (Idle,(Hex(0,0,0),0),vstack)
    stopGraphics
