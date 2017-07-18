module Hpcb.Component.Resistor (
  r1206,
  r805
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Effects
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

-- | SMD Resistor, 1206 package (3216 metric)
r1206 ::  String        -- ^ Reference
          -> String     -- ^ Value
          -> Footprint
r1206 ref val = footprint "R_1206" $
  fpText "reference" ref StandardEffects # translate (V2 0 (-2.3)) # layer FSilkS
  <> fpText "value" val StandardEffects # translate (V2 0 2.3) # layer FFab
  <> fpRectangle 3.2 1.6 0.1 # layer FFab
  <> fpRectangle 4.4 2.4 0.05 # layer FCrtYd
  <> (
    fpLine (V2 1 1.075) (V2 (-1) 1.075) 0.15
    <> fpLine (V2 (-1) (-1.075)) (V2 1 (-1.075)) 0.15
  ) # layer FSilkS
  <> (
    pad 1 SMD Rect (V2 0.9 1.7) (newNet ref 1) # translate (V2 (-1.45) 0)
    <> pad 2 SMD Rect (V2 0.9 1.7) (newNet ref 2) # translate (V2 1.45 0)
  ) # layers [FCu, FPaste, FMask]

-- | SMD Resistor, 805 package (2012 metric)
r805 :: String      -- ^ Reference
        -> String   -- ^ Value
        -> Footprint
r805 ref val = footprint "R_805" $
  fpText "reference" ref StandardEffects # translate (V2 0 (-2.3)) # layer FSilkS
  <> fpText "value" val StandardEffects # translate (V2 0 2.3) # layer FFab
  <> fpRectangle 2.0 1.25 0.1 # layer FFab
  <> fpRectangle 3.2 2.0 0.05 # layer FCrtYd
  <> (
    fpLine (V2 0.6 0.875) (V2 (-0.6) 0.875) 0.15
    <> fpLine (V2 (-0.6) (-0.875)) (V2 0.6 (-0.875)) 0.15
  ) # layer FSilkS
  <> (
    pad 1 SMD Rect (V2 0.7 1.3) (newNet ref 1) # translate (V2 (-0.95) 0)
    <> pad 2 SMD Rect (V2 0.7 1.3) (newNet ref 2) # translate (V2 0.95 0)
  ) # layers [FCu, FPaste, FMask]
