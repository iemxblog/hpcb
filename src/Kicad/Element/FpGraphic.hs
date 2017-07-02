module Kicad.Element.FpGraphic (
  FpGraphic(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.Effects

data FpGraphic =
  FpLine (V2 Float) (V2 Float) Layer Float -- ^ line start, line end, layer, line width
  | FpCircle (V2 Float) (V2 Float) Layer Float -- ^ center, end, layer, width
  | FpText String String Position Layer Effects -- ^ name, content, position, layer, effects (font, justification , etc.)

instance Itemizable FpGraphic where
  itemize (FpLine (V2 xs ys) (V2 xe ye) l w) =
    Item "fp_line" [
      Item "start" [PFloat xs, PFloat ys] ,
      Item "end" [PFloat xe, PFloat ye],
      itemize l, PFloat w
    ]

  itemize (FpText name text pos layer effects) =
    Item "fptext" [
      PString name,
      PString text,
      itemize pos,
      itemize layer,
      itemize effects
      ]
