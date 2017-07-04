module Kicad.Element.Action (
  Transformable(..),
  ChangeableLayer(..)
) where

import Kicad.Element.Base
import Kicad.Element.Layer

class Transformable a where
  transform :: (Position -> Position) -> a -> a

  translate :: V2 Float -> a -> a
  translate = transform . translation

  rotate :: Float -> a -> a
  rotate = transform . rotation

class ChangeableLayer a where
  layer :: Layer -> a -> a
  layers :: [Layer] -> a -> a
