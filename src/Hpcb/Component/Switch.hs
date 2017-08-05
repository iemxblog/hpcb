module Hpcb.Component.Switch (
  tact_switch
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.Layer
import Hpcb.Data.FpElement
import Hpcb.Functions
import Data.Monoid

tact_switch ::  String      -- ^ Reference
                -> Circuit
tact_switch ref = footprint ref "TACT_SWITCH" $
  fpText "reference" ref defaultEffects # translate (V2 0 (-4)) # layer FSilkS
  <> fpText "value" "TACT_SWITCH" defaultEffects # translate (V2 0 4) # layer FFab
  <> (
    fpLine (V2 (-2) (-3)) (V2 2 (-3))
    <> fpLine (V2 (-2) 3) (V2 2 3)
    <> fpLine (V2 (-3) (-1)) (V2 (-3) 1)
    <> fpLine (V2 3 (-1)) (V2 3 1)

  ) # layer FSilkS # width 0.15
  <> fpCircle 3 # layer FFab # width 0.15
  <> fpRectangle 8.7 6.7 # layer FCrtYd # width 0.05
  <> (
    pad 1 ThroughHole{getDrill=1} Circle (V2 1.7272 1.7272) (newNet ref 1) # translate (V2 (-6.5/2) (-4.5/2))
    <> pad 1 ThroughHole{getDrill=1} Circle (V2 1.7272 1.7272) (newNet ref 1) # translate (V2 (6.5/2) (-4.5/2))
    <> pad 2 ThroughHole{getDrill=1} Circle (V2 1.7272 1.7272) (newNet ref 2) # translate (V2 (-6.5/2) (4.5/2))
    <> pad 2 ThroughHole{getDrill=1} Circle (V2 1.7272 1.7272) (newNet ref 2) # translate (V2 (6.5/2) (4.5/2))

  ) # layers (copperLayers ++ maskLayers)
