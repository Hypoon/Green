module RGB
( RGB(..)
, lighten
, darken
, typeToRGB
) where

import Hex
import Data.Word

type RGB = (Word8,Word8,Word8)

lighten :: RGB -> RGB
lighten (r,g,b) = (0x80+(r `quot` 2),0x80+(g `quot` 2),0x80+(b `quot` 2))

darken :: RGB -> RGB
darken (r,g,b) = (r `quot` 2, g `quot` 2, b `quot` 2)

typeToRGB :: Type -> RGB
typeToRGB (Type (Light,c)) = lighten (typeToRGB (Type (Shadeless,c)))
typeToRGB (Type (Dark,c)) = darken (typeToRGB (Type (Shadeless,c)))
typeToRGB (Type (Shadeless,Red)) = (0xFF,0,0)
typeToRGB (Type (Shadeless,Green)) = (0,0xFF,0)
typeToRGB (Type (Shadeless,Blue)) = (0,0,0xFF)
typeToRGB (Type (Shadeless,Colorless)) = (0x80,0x80,0x80)
