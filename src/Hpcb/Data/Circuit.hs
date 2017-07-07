module Hpcb.Data.Circuit(
  Circuit(..)
) where

import Hpcb.Data.Footprint
import Hpcb.Data.Graphic
import Hpcb.Data.Segment

data Circuit = Circuit [Footprint] [Graphic] [Segment]

instance Monoid Circuit where
  mempty = Circuit [] [] []
  (Circuit m1 g1 s1) `mappend` (Circuit m2 g2 s2) = Circuit (m1 ++ m2) (g1 ++ g2) (s1 ++ s2)
