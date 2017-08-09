module Hpcb.Data.Action (
  Transformable(..),
  Parameterized(..)
) where

import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Effects
import Data.Matrix

class Transformable a where
  transform :: Matrix Float -> a -> a

  translate :: V2 Float -> a -> a
  translate = transform . translation

  rotate :: Float -> a -> a
  rotate = transform . rotation

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
