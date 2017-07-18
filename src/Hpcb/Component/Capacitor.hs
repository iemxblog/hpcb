module Hpcb.Component.Capacitor (
  c805
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Effects
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

-- | SMD Capacitor, 805 package (2012 metric)
c805 :: String        -- ^ Reference
        -> String     -- ^ Value
        -> Footprint
c805 ref val = footprint "C_0805" $
  fpText "reference" ref StandardEffects # translate (V2 0 (-2.1)) # layer FSilkS
  <> fpText "value" val StandardEffects # translate (V2 0 2.1) # layer FFab
  <> fpRectangle 2.0 1.25 0.15 # layer FFab
  <> fpRectangle 3.6 2.0 0.05 # layer FCrtYd
  <> (
    fpLine (V2 0.5 (-0.85)) (V2 (-0.5) (-0.85)) 0.15
    <> fpLine (V2 (-0.5) 0.85) (V2 0.5 0.85) 0.15
  ) # layer FSilkS
  <> (
    pad 1 SMD Rect (V2 1 1.25) (newNet ref 1) # translate (V2 (-1) 0)
    <> pad 2 SMD Rect (V2 1 1.25) (newNet ref 2) # translate (V2 1 0)
  ) # layers [FCu, FPaste, FMask]
