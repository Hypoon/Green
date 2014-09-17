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

renderVisual :: Visual -> Frame -> Time -> Renderable
renderVisual Dot _ _ = renderDot
renderVisual DebugText (centerHex,rotation) _ = (renderText (hexToText centerHex) (screen_width,screen_height) (0,0,0))
renderVisual (Ground hextille) (centerHex,rotation) _ = (renderHextille hextille (centerHex,rotation))

renderAll :: VisualStack -> Frame -> Time -> IO()
renderAll vstack (centerHex,rotation) time = do
    let stack = map (\visual -> renderVisual visual (centerHex,rotation) time) vstack
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
actionToStatus (DoMove dir) (oldHex,rotation) now = Moving oldHex (oldHex `addHex` dir) now
actionToStatus (DoRotate angle) (axis,oldrotation) now = Rotating oldrotation (oldrotation + angle) now

run :: (Status,Frame,VisualStack) -> IO()
run (Idle,(centerHex,rotation),vstack) = do
    now <- getTicks
    renderAll vstack (centerHex,rotation) now
    key <- getKey
    let action = (processChar key)
    let newStatus = actionToStatus action (centerHex,rotation) now
    run (newStatus,(centerHex,rotation),vstack)
run (Quitting,_,_) = return()
run (Moving src dest startTime,(centerHex,rotation),vstack) = do
    now <- getTicks
    let interval = now - startTime
    if (interval < 500)
        then do
            let newCenterHex = (src `addHex` ( ((fromIntegral interval)/500.0) `scaleHex` (dest `subHex` src) ))
            renderAll vstack (newCenterHex,rotation) now
            run (Moving src dest startTime,(newCenterHex,rotation),vstack)
        else do
            let newCenterHex = dest
            renderAll vstack (newCenterHex,rotation) now
            run (Idle,(newCenterHex,rotation),vstack)
run (Rotating src dest startTime,(centerHex,rotation),vstack) = do
    now <- getTicks
    let interval = now - startTime
    if (interval < 500)
        then do
            let newRotation = (src + (((fromIntegral interval)/500.0)*(dest-src)))
            renderAll vstack (centerHex,newRotation) now
            run (Rotating src dest startTime,(centerHex,newRotation),vstack)
        else do
            let newRotation = dest
            renderAll vstack (centerHex,newRotation) now
            run (Idle,(centerHex,newRotation),vstack)
{-
    let ((Ground hextille):restofvstack) = vstack
    let newHextille = map (\(h,t) -> ((rotateHex h centerHex 1),t)) hextille
    let newvstack = ((Ground newHextille):restofvstack)
    run (Idle,(centerHex,rotation),newvstack)
 -}          


main :: IO()
main = do
    startGraphics
    let rh = randomHextille 0 2
    let vstack =    ( Ground rh
                    : Dot
                    : DebugText
                    : [])
    run (Idle,(Hex(0,0,0),0),vstack)
    stopGraphics
