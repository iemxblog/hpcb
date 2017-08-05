module Hpcb.Component.Resistor (
  r1206,
  r805
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

-- | SMD Resistor, 1206 package (3216 metric)
r1206 ::  String        -- ^ Reference
          -> String     -- ^ Value
          -> Circuit
r1206 ref val = footprint ref "R_1206" $
  fpText "reference" ref defaultEffects # translate (V2 0 (-2.3)) # layer FSilkS
  <> fpText "value" val defaultEffects # translate (V2 0 2.3) # layer FFab
  <> fpRectangle 3.2 1.6 # layer FFab # width 0.1
  <> fpRectangle 4.4 2.4 # layer FCrtYd # width 0.05
  <> (
    fpLine (V2 1 1.075) (V2 (-1) 1.075)
    <> fpLine (V2 (-1) (-1.075)) (V2 1 (-1.075))
  ) # layer FSilkS # width 0.15
  <> (
    pad 1 SMD Rect (V2 0.9 1.7) (newNet ref 1) # translate (V2 (-1.45) 0)
    <> pad 2 SMD Rect (V2 0.9 1.7) (newNet ref 2) # translate (V2 1.45 0)
  ) # layers [FCu, FPaste, FMask]

-- | SMD Resistor, 805 package (2012 metric)
r805 :: String      -- ^ Reference
        -> String   -- ^ Value
        -> Circuit
r805 ref val = footprint ref "R_805" $
  fpText "reference" ref defaultEffects # translate (V2 0 (-2.3)) # layer FSilkS
  <> fpText "value" val defaultEffects # translate (V2 0 2.3) # layer FFab
  <> fpRectangle 2.0 1.25 # layer FFab # width 0.1
  <> fpRectangle 3.2 2.0 # layer FCrtYd # width 0.05
  <> (
    fpLine (V2 0.6 0.875) (V2 (-0.6) 0.875)
    <> fpLine (V2 (-0.6) (-0.875)) (V2 0.6 (-0.875))
  ) # layer FSilkS # width 0.15
  <> (
    pad 1 SMD Rect (V2 0.7 1.3) (newNet ref 1) # translate (V2 (-0.95) 0)
    <> pad 2 SMD Rect (V2 0.7 1.3) (newNet ref 2) # translate (V2 0.95 0)
  ) # layers [FCu, FPaste, FMask]
