module Kicad.Element.Pad (
  Pad(..),
  PadType(..),
  PadShape(..),
  PadDrill(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.Net

data Pad = Pad Int PadType PadShape Position Size PadDrill [Layer] Net  -- ^ Int : Pin number

data PadType = ThroughHole | SMD
instance Itemizable PadType where
  itemize ThroughHole = PString "thru_hole"
  itemize SMD = PString "smd"

data PadShape = Circle | Rect | Oval
instance Itemizable PadShape where
  itemize Circle = PString "circle"
  itemize Rect = PString "rect"
  itemize Oval = PString "oval"

newtype PadDrill = PadDrill Float
instance Itemizable PadDrill where
  itemize (PadDrill f) = Item "drill" [PFloat f]

instance Itemizable Pad where
  itemize (Pad number padType shape pos size drill layers net) =
    Item "Pad" [PInt number, itemize padType, itemize shape, itemize pos, itemize size, itemize drill, itemize layers, itemize net]
