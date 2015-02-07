import Hex
import Graphics
import Input
import Time

import Numeric

import Interface

main :: IO()
main = do
    let rh = randomHextille 0 2
    startInterface rh
