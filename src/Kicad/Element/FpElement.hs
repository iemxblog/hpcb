module Kicad.Element.FpElement (
  FpElement(..),
  PadType(..),
  PadShape(..),
  PadDrill(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.Effects
import Kicad.Element.Net

data FpElement =
  FpLine (V2 Float) (V2 Float) Layer Float -- ^ line start, line end, layer, line width
  | FpCircle (V2 Float) (V2 Float) Layer Float -- ^ center, end, layer, width
  | FpText String String Position Layer Effects -- ^ name, content, position, layer, effects (font, justification , etc.)
  | Pad Int PadType PadShape Position Size PadDrill [Layer] Net  -- ^ Int : Pin number

instance Itemizable FpElement where
  itemize (FpLine (V2 xs ys) (V2 xe ye) l w) =
    Item "fp_line" [
      Item "start" [PFloat xs, PFloat ys] ,
      Item "end" [PFloat xe, PFloat ye],
      itemize l,
      Item "width" [PFloat w]
    ]

  itemize (FpCircle (V2 xc yc) (V2 xe ye) l w) =
    Item "fp_circle" [
      Item "center" [PFloat xc, PFloat yc],
      Item "end" [PFloat xe, PFloat ye],
      itemize l,
      Item "width" [PFloat w]
    ]

  itemize (FpText name text pos layer effects) =
    Item "fptext" [
      PString name,
      PString text,
      itemize pos,
      itemize layer,
      itemize effects
    ]

  itemize (Pad number padType shape pos size drill layers net) =
    Item "Pad" [
      PInt number,
      itemize padType,
      itemize shape,
      itemize pos,
      itemize size,
      itemize drill,
      itemize layers,
      itemize net
    ]


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
