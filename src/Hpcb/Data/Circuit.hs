module Hpcb.Data.Circuit(
  Circuit(..),
  _footprints,
  _graphics,
  _segments
) where

import Hpcb.Data.Footprint
import Hpcb.Data.Graphic
import Hpcb.Data.Segment
import Hpcb.SExpr
import Control.Lens

data Circuit = Circuit {
  getFootprints :: [Footprint],
  getGraphics :: [Graphic],
  getSegments :: [Segment]
} deriving Show

instance Monoid Circuit where
  mempty = Circuit [] [] []
  (Circuit m1 g1 s1) `mappend` (Circuit m2 g2 s2) = Circuit (m1 ++ m2) (g1 ++ g2) (s1 ++ s2)

instance Itemizable Circuit where
  itemize (Circuit f g s) = Item "kicad_pcb" $
    map itemize f ++ map itemize g ++ map itemize s

-- Lenses
_footprints :: Lens' Circuit [Footprint]
_footprints = lens getFootprints (\circuit footprints -> circuit {getFootprints = footprints})

_graphics :: Lens' Circuit [Graphic]
_graphics = lens getGraphics (\circuit graphics -> circuit {getGraphics = graphics})

_segments :: Lens' Circuit [Segment]
_segments = lens getSegments (\circuit segments -> circuit {getSegments = segments})
