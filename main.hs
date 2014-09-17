import Hex
import Graphics
import Input
import Time

processChar :: Char -> Hextille -> Hex -> IO()
processChar '\ESC' _ _ = return()
processChar ' ' hextille centerhex = do
    showEverything hextille centerhex
    run hextille centerhex
processChar c hextille centerhex = do
    nowTick <- getTicks
    let dir = charToHex c
    animateMotion hextille centerhex nowTick nowTick dir
    run hextille (centerhex `addHex` dir)

run :: Hextille -> Hex -> IO()
run hextille centerhex = do
    key <- getKey
    processChar key hextille centerhex

main :: IO()
main = do
    startGraphics
    let rh = randomHextille 0 2
    run rh (Hex(0,0,0))
    stopGraphics
