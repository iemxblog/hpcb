module Hpcb.Data.Segment (
  Segment(..),
  _segNet
) where

import Hpcb.Data.Action
import Hpcb.SExpr
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Net
import Control.Lens

data Segment = Segment {
  getSegstart :: V2 Float,
  getSegend :: V2 Float,
  getSegwidth :: Float,
  getSeglayer :: Layer,
  getSegnet :: Net
} deriving Show

instance Itemizable Segment where
  itemize (Segment (V2 xs ys) (V2 xe ye) w l n) =
    Item "segment" [
      Item "start" [PFloat xs, PFloat ys],
      Item "end" [PFloat xe, PFloat ye],
      Item "widh" [PFloat w],
      itemize l,
      itemize n
    ]

instance ChangeableLayer Segment where
  layer l (Segment s e w _ n) = Segment s e w l n
  layers ls Segment{} = error "A segment cannot have multiple layers"

-- Lenses
_segNet :: Lens' Segment Net
_segNet = lens getSegnet (\segment net -> segment {getSegnet = net})
