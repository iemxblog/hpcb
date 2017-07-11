module Hpcb.Data.Segment (
  Segment(..),
  net
) where

import Hpcb.SExpr
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Net
import Control.Lens

data Segment = Segment {
  _segstart :: V2 Float,
  _segend :: V2 Float,
  _segwidth :: Float,
  _seglayer :: Layer,
  _segnet :: Net
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

-- Lenses
net :: Lens' Segment Net
net = lens _segnet (\segment net -> segment {_segnet = net})
