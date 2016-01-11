module Hex
( Hex(..)
, bigLoc
, smallLoc
, addHex
, negateHex
, subHex
, scaleHex
, Color(..)
, Shade(..)
, Type(..)
, hexLine
, hexRing
, hexDisk
, Hextille(..)
, randomHextille
, rotateHex
, rotateHexAboutOrigin
, roundHex
) where

import System.Random
import Control.Applicative

newtype Hex = Hex (Double,Double,Double) deriving (Eq, Show)

bigLoc :: Hex -> (Double,Double,Double)
bigLoc (Hex (u,v,w)) = (v-w,w-u,u-v)

smallLoc :: Hex -> (Double,Double,Double)
smallLoc (Hex (u,v,w)) = (u,v,w)

addHex :: Hex -> Hex -> Hex
addHex (Hex (u1,v1,w1)) (Hex (u2,v2,w2)) = Hex (u1+u2,v1+v2,w1+w2)

negateHex :: Hex -> Hex
negateHex (Hex (u,v,w)) = Hex(-u,-v,-w)

subHex :: Hex -> Hex -> Hex
subHex a b = addHex a (negateHex b)

scaleHex :: Double -> Hex -> Hex
scaleHex k (Hex (u,v,w)) = Hex (k*u,k*v,k*w)

roundHex :: Hex -> Hex
roundHex (Hex (u,v,w))
    | (m == x) = (Hex (fromIntegral $ round u, fromIntegral $ round v, fromIntegral $ -(round u)-(round v)))
    | (m == y) = (Hex (fromIntegral $ -(round v)-(round w), fromIntegral $ round v, fromIntegral $ round w))
    | (m == z) = (Hex (fromIntegral $ round u, fromIntegral $ -(round w)-(round u), fromIntegral $ round w))
    where
        x = (fromIntegral $ round u) + (fromIntegral $ round v) + w
        y = (fromIntegral $ round v) + (fromIntegral $ round w) + u
        z = (fromIntegral $ round w) + (fromIntegral $ round u) + v
        m = minimum [x,y,z]

data Color = Red | Green | Blue | Colorless deriving Eq

instance Show Color where
    show Colorless = "Neutral"
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

colorToInt :: Color -> Int
colorToInt Colorless = 0
colorToInt Red = 1
colorToInt Green = 2
colorToInt Blue = 3

intToColor :: Int -> Color
intToColor 1 = Red
intToColor 2 = Green
intToColor 3 = Blue
intToColor _ = Colorless

instance Random Color where
    randomR (a,b) g = case (randomR(colorToInt a, colorToInt b) g) of (x,g') -> (intToColor x,g')
    random g = randomR(Colorless,Blue) g

data Shade = Light | Dark | Shadeless deriving Eq

instance Show Shade where
    show Shadeless = "Neutral"
    show Light = "Light"
    show Dark = "Dark"

shadeToInt :: Shade -> Int
shadeToInt Shadeless = 0
shadeToInt Dark = 1
shadeToInt Light = 2

intToShade :: Int -> Shade
intToShade 1 = Dark
intToShade 2 = Light
intToShade _ = Shadeless

instance Random Shade where
    randomR (a,b) g = case (randomR(shadeToInt a, shadeToInt b) g) of (x,g') -> (intToShade x,g')
    random g = randomR(Shadeless,Light) g

data Type = Type (Shade, Color) deriving Eq

instance Show Type where
    show (Type (Shadeless,Colorless)) = "Neutral"
    show (Type (Shadeless,c)) = show c
    show (Type (s,Colorless)) = show s
    show (Type (s,c)) = (show s) ++ (show c)

typeToInt :: Type -> Int
typeToInt (Type (s,c)) = (colorToInt c)+4*(shadeToInt s)

intToType :: Int -> Type
intToType x = Type (intToShade (quot x 4), intToColor (rem x 4))

instance Random Type where
    randomR (a,b) g = case (randomR(typeToInt a, typeToInt b) g) of (x,g') -> (intToType x,g')
    random g = randomR(Type(Shadeless,Colorless),Type(Light,Blue)) g

hexLine :: Hex -> Hex -> Int -> [Hex]
hexLine start direction length = map (\l -> addHex start (scaleHex ((fromIntegral l)::Double) direction)) [0..length]

hexRing :: Int -> [Hex]
hexRing 0 = [Hex(0,0,0)]
hexRing r = concat (map (side r) [0..5]) where
    side r 0 = hexLine (scaleHex (fromIntegral r) (Hex(0,1,-1))) (Hex(-1,0,1)) (r-1)
    side r 1 = hexLine (scaleHex (fromIntegral r) (Hex(-1,1,0))) (Hex(0,-1,1)) (r-1)
    side r 2 = hexLine (scaleHex (fromIntegral r) (Hex(-1,0,1))) (Hex(1,-1,0)) (r-1)
    side r 3 = hexLine (scaleHex (fromIntegral r) (Hex(0,-1,1))) (Hex(1,0,-1)) (r-1)
    side r 4 = hexLine (scaleHex (fromIntegral r) (Hex(1,-1,0))) (Hex(0,1,-1)) (r-1)
    side r 5 = hexLine (scaleHex (fromIntegral r) (Hex(1,0,-1))) (Hex(-1,1,0)) (r-1)

hexDisk :: Int -> [Hex]
hexDisk r = concat (map hexRing [0..r])

type Hextille = [(Hex,Type)]

randomHextille :: Int -> Int -> Hextille
randomHextille seed radius = 
    let ts = take area $ randoms (mkStdGen seed) :: [Type] where
            area = 1+6*(sum [1..radius])
        hs = hexDisk radius
    in zip hs ts

{-
rotateHexAboutOrigin :: Hex -> Int -> Hex
rotateHexAboutOrigin hex 0 = hex
rotateHexAboutOrigin (Hex(u,v,w)) 1 = (Hex(-w,-u,-v))
rotateHexAboutOrigin (Hex(u,v,w)) (-1) = (Hex(-v,-w,-u))
rotateHexAboutOrigin hex amt
    | amt > 1 = rotateHexAboutOrigin (rotateHexAboutOrigin hex (amt-1)) 1
    | amt < (-1) = rotateHexAboutOrigin (rotateHexAboutOrigin hex (amt+1)) (-1)
-}

rotateHexAboutOrigin :: Hex -> Double -> Hex
rotateHexAboutOrigin (Hex(u,v,w)) angle = (Hex(nu,nv,nw)) where
    theta = angle*pi/3.0
    nu = ((-1)*(v+w)*(cos theta))+((v-w)*(sin theta)*(sqrt (1/3)))
    nv = ((-1)*(w+u)*(cos theta))+((w-u)*(sin theta)*(sqrt (1/3)))
    nw = ((-1)*(u+v)*(cos theta))+((u-v)*(sin theta)*(sqrt (1/3)))

rotateHex :: Hex -> Hex -> Double -> Hex
rotateHex hex origin amount = addHex origin $ (rotateHexAboutOrigin (hex `subHex` origin) amount)

{-
padShowType :: Type -> String
padShowType t = begin ++ (show t) ++ end where
    begin = take (quot (10 - (length (show t))) 2) $ repeat ' '
    end = take (10 - (length (show t)) - (length begin)) $ repeat ' '

showType :: Type -> String
showType t = "  ________  " ++ "\n" ++ " /        \\ " ++ "\n" ++ "/" ++ (padShowType t) ++ "\\" ++ "\n" ++ "\\          /" ++ "\n" ++ " \\________/ "

blank :: Int -> String
blank r = let w = 8*(2*r+1)+4*r+4
              h = 4*(2*r+1)+1
          in unlines (take h (repeat (take w (repeat ' '))))

showHex :: (Hex,Type) -> Int -> String
showHex (Hex(u,v,w),t) r = unlines $ before++mid++after where
    before=take (r*4-2*(v-w)) (repeat (take (8*(2*r+1)+4*r+4) (repeat ' ')))
    mid=map (++(take (r*10+u*10) (repeat ' '))) (map ((take (r*10-u*10) (repeat ' '))++) (lines (showType t)))
    after=take (r*4+2*(v-w)) (repeat (take (8*(2*r+1)+4*r+4) (repeat ' ')))

superimpose :: String -> String -> String
superimpose a b = getZipList $ max <$> ZipList a <*> ZipList b

showHextille :: Hextille -> String
showHextille [] = ""
showHextille h = foldr superimpose (blank r) (map (\hex -> showHex hex r) h) where
    r = maximum (map (\ (Hex(u,v,w)) -> maximum [u,v,w]) (map fst h))

-}
