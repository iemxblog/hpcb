module Kicad.Element.FpLine(
  FpLine(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer

data FpLine = FpLine
  (V2 Float)  -- ^ Line start
  (V2 Float)  -- ^ Line end
  Layer       -- ^ Layer
  Float       -- ^ Line width

instance Itemizable FpLine where
  itemize (FpLine (V2 xs ys) (V2 xe ye) l w) = Item "fp_line" [Item "start" [PFloat xs, PFloat ys] , Item "end" [PFloat xe, PFloat ye], itemize l, PFloat w]
