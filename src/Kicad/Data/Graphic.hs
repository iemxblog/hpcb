module Kicad.Data.Graphic (
  Graphic
) where

import Kicad.SExpr
import Kicad.Data.Base
import Kicad.Data.Layer
import Kicad.Data.Effects

data Graphic =
  GrLine (V2 Float) (V2 Float) Float Layer Float  -- ^ start, end, angle, layer, width
  | GrCircle (V2 Float) (V2 Float) Layer Float    -- ^ center, end, layer, width
  | GrText String Position Layer Effects

instance Itemizable Graphic where
  itemize (GrLine (V2 xs ys) (V2 xe ye) a l w) =
    Item "gr_line" [
      Item "start" [PFloat xs, PFloat ys],
      Item "end" [PFloat xe, PFloat ye],
      Item "angle" [PFloat a],
      itemize l,
      Item "width" [PFloat w]
      ]

  itemize (GrCircle (V2 xc yc) (V2 xe ye) l w) =
    Item "gr_circle" [
      Item "center" [PFloat xc, PFloat yc],
      Item "end" [PFloat xe, PFloat ye],
      itemize l,
      Item "width" [PFloat w]
    ]

  itemize (GrText s pos l e) =
    Item "gr_text" [
      PString $ show s,
      itemize pos,
      itemize l,
      itemize e
    ]
