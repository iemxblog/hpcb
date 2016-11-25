module Gerber (
    Aperture(..)
    , formatFloat 
) where

import Numeric

coordinateFormat = (3, 6)

data Aperture = 
    Circle Float (Maybe Float) -- Diameter, hole diamater
    | Rectangle Float Float (Maybe Float) -- X size, Y size, hole diameter
    | Obround Float Float (Maybe Float) -- X size, Y size, hole dimater (X size and Y size are the dimensions of the enclosing box)
    | Polygon Float Float (Maybe (Float, Maybe Float)) -- Outer diameter, number of vertices, rotation, hole diameter
    deriving Eq


removeZeros :: String -> String
removeZeros [] = []
removeZeros [x] = [x]
removeZeros ('0':xs) = removeZeros xs
removeZeros xs = xs

formatFloat :: Float -> String
formatFloat f = removeZeros . filter (/= '.') $ showFFloat (Just (snd coordinateFormat)) f ""

-- Format float for aperture definition
formatFloatA :: Float -> String
formatFloatA f = showFFloat (Just 5) f ""

