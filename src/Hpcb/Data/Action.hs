module Hpcb.Data.Action (
  Transformable(..),
  Parameterized(..)
) where

import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Effects
import Data.Matrix

-- | Class of types that can be transformed by geometric operations.
class Transformable a where
  transform :: Matrix Float -> a -> a

  translate :: V2 Float -> a -> a
  translate = transform . translation

  rotate :: Float -> a -> a
  rotate = transform . rotation

  reflectX :: a -> a
  reflectX = transform reflectionX

  reflectY :: a -> a
  reflectY = transform reflectionY

-- | Class of types that have parameters that can be modified
-- (layer, width, font, etc.)
class Parameterized a where
  layer :: Layer -> a -> a
  layers :: [Layer] -> a -> a
  width :: Float -> a -> a
  effects :: (Effects -> Effects) -> a -> a
  fontSize :: (Float, Float) -> a -> a
  fontSize = effects . fontSizeE
  fontThickness :: Float -> a -> a
  fontThickness = effects . fontThicknessE
  fontStyle :: Style -> a -> a
  fontStyle = effects . fontStyleE
  justify :: Justification -> a -> a
  justify = effects . justifyE
  mirror :: a -> a
  mirror = effects mirrorE
