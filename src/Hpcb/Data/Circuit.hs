module Hpcb.Data.Circuit(
  PCB
) where

import Hpcb.Data.Footprint
import Hpcb.Data.Graphic
import Hpcb.Data.Segment

data PCB = PCB [Footprint] [Graphic] [Segment]

instance Monoid PCB where
  mempty = PCB [] [] []
  mappend (PCB m1 g1 s1) (PCB m2 g2 s2) = PCB (m1 ++ m2) (g1 ++ g2) (s1 ++ s2)
