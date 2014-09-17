import Hex
import Graphics
import Input
import Time

data Status = Quitting | Moving Hex Hex Time | Idle
data Action = DoQuit | DoIdle | DoMove Hex
data Visual = DebugText | Dot | Ground Hextille
type VisualStack = [Visual]

renderVisual :: Visual -> Hex -> Time -> Renderable
renderVisual Dot _ _ = renderDot
renderVisual DebugText centerHex _ = (renderText (show (smallLoc centerHex)) (screen_width,screen_height) (0,0,0))
renderVisual (Ground hextille) centerHex _ = (renderHextille hextille centerHex)

renderAll :: VisualStack -> Hex -> Time -> IO()
renderAll vstack centerHex time = do
    let stack = map (\visual -> renderVisual visual centerHex time) vstack
    displayAll stack

{-
standStill :: Hextille -> Hex -> IO()
standStill hextille hex = do
    time <- getTicks
    displayAll  ( (renderHextille hextille centerhex)
                : (renderDot)
                : (renderText (show (smallLoc centerhex)) (screen_width,screen_height) (0,0,0))
                : []) time
    run hextille centerhex

quit :: IO()
quit = return()
-}
--move :: Hextille -> Hex -> Hex -> Time -> IO()
--move hextille centerhex dir startTime = 

{-
processChar :: Char -> Hextille -> Hex -> IO()
processChar '\ESC' _ _ = quit
processChar ' ' hextille centerhex = do
    standStill hextile centerhex
processChar c hextille centerhex = do
    time <- getTicks
    let dir = charToHex c
        new = (centerhex `addHex` dir)
    --animateMotion hextille centerhex time time dir
    displayAll  ( (renderSlidingHextille hextille centerhex new startTime)
                : []) time
    run hextille new
-}

processChar :: Char -> Action
processChar '\ESC' = DoQuit
processChar ' ' = DoIdle
processChar char = DoMove (charToHex char)

actionToStatus :: Action -> Hex -> Time -> Status
actionToStatus DoQuit _ _ = Quitting
actionToStatus DoIdle _ _ = Idle
actionToStatus (DoMove dir) oldHex now = Moving oldHex (oldHex `addHex` dir) now

--run :: Hextille -> Hex -> IO()
--run hextille centerhex = do
--    key <- getKey
--    processChar key hextille centerhex

run :: (Status,Hex,VisualStack) -> IO()
run (Idle,centerHex,vstack) = do
    now <- getTicks
    renderAll vstack centerHex now
    key <- getKey
    let action = (processChar key)
    let newStatus = actionToStatus action centerHex now
    run (newStatus,centerHex,vstack)
run (Quitting,_,_) = return()
run (Moving src dest startTime,centerHex,vstack) = do
    now <- getTicks
    let interval = now - startTime
    if (interval < 500)
        then do
            let newCenterHex = (src `addHex` ( ((fromIntegral interval)/500.0) `scaleHex` (dest `subHex` src) ))
            renderAll vstack newCenterHex now
            run (Moving src dest startTime,newCenterHex,vstack)
        else do
            let newCenterHex = dest
            renderAll vstack newCenterHex now
            run (Idle,newCenterHex,vstack)
            


main :: IO()
main = do
    startGraphics
    let rh = randomHextille 0 2
    let vstack =    ( Ground rh
                    : Dot
                    : DebugText
                    : [])
{-    let stack = ( (renderHextille rh (Hex(0,0,0)))
                : renderDot
                : (renderText (show (smallLoc (Hex(0,0,0)))) (screen_width,screen_height) (0,0,0))
                : [])-}
    run (Idle,Hex(0,0,0),vstack)
    stopGraphics
