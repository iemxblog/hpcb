module Hpcb.Component.SOT (
  sot23
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

sot23 ::  String
          -> String
          -> Circuit
sot23 ref val = footprint ref "SOT_23" $
  fpText "reference" ref StandardEffects # translate (V2 0 (-2.5)) # layer FSilkS
  <> fpText "value" val StandardEffects # translate (V2 0 2.5) # layer FFab
  <> fpPolygon [
    V2 (-0.7) (-0.95),
    V2 (-0.15) (-1.52),
    V2 0.7 (-1.52),
    V2 0.7 1.52,
    V2 (-0.7) 1.52
  ] # layer FFab # width 0.1
  <> fpRectangle 3.4 3.5 # layer FCrtYd # width 0.05
  <> (
    fpLine (V2 0.76 1.58) (V2 0.76 0.65)
    <> fpLine (V2 0.76 (-1.58)) (V2 0.76 (-0.65))
    <> fpLine (V2 0.76 (-1.58)) (V2 (-1.4) (-1.58))
    <> fpLine (V2 0.76 1.58) (V2 (-0.7) 1.58)
  ) # layer FSilkS # width 0.12
  <> (
    pad 1 SMD Rect (V2 0.9 0.8) (newNet ref 1) # translate (V2 (-1) (-0.95))
    <> pad 2 SMD Rect (V2 0.9 0.8) (newNet ref 2) # translate (V2 (-1) 0.95)
    <> pad 3 SMD Rect (V2 0.9 0.8) (newNet ref 3) # translate (V2 1 0)
  ) # layers [FCu, FPaste, FMask]
