import Hex
import Graphics

main = do
    initialize (screen_width,screen_height)
    let rh = randomHextille 0 1
    run rh (Hex(0,0,0))
    close
