module Hpcb.Data.Segment (
  Segment(..)
) where

import Hpcb.SExpr
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Net

data Segment = Segment (V2 Float) (V2 Float) Float Layer Net  -- ^ start, end, width, layer, net

instance Itemizable Segment where
  itemize (Segment (V2 xs ys) (V2 xe ye) w l n) =
    Item "segment" [
      Item "start" [PFloat xs, PFloat ys],
      Item "end" [PFloat xe, PFloat ye],
      Item "widh" [PFloat w],
      itemize l,
      itemize n
    ]
