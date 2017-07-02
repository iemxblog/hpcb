module Kicad.Element.PCB(
  PCB
) where

import Kicad.Element.Footprint
import Kicad.Element.Graphic
import Kicad.Element.Segment

data PCB = PCB [Footprint] [Graphic] [Segment]

instance Monoid PCB where
  mempty = PCB [] [] []
  mappend (PCB m1 g1 s1) (PCB m2 g2 s2) = PCB (m1 ++ m2) (g1 ++ g2) (s1 ++ s2)
