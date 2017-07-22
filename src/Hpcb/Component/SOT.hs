module Hpcb.Component.SOT (
  sot_23,
  sot_23_5
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

sot_23 ::  String      -- ^ Reference
          -> String   -- ^ Value
          -> Circuit
sot_23 ref val = footprint ref "SOT-23" $
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


sot_23_5 ::  String      -- ^ Reference
            -> String   -- ^ Value
            -> Circuit
sot_23_5 ref val = footprint ref "SOT-23-5" $
  fpText "reference" ref StandardEffects # translate (V2 0 (-2.9)) # layer FSilkS
  <> fpText "value" val StandardEffects # translate (V2 0 2.9) # layer FFab
  <> (
    fpLine (V2 (-0.9) 1.61) (V2 0.9 1.61)
    <> fpLine (V2 0.9 (-1.61)) (V2 (-1.55) (-1.61))
  ) # layer FSilkS # width 0.12
  <> fpRectangle 3.8 3.6 # layer FCrtYd # width 0.05
  <> fpPolygon [
    V2 (-0.9) (-0.9),
    V2 (-0.25) (-1.55),
    V2 0.9 (-1.55),
    V2 0.9 1.55,
    V2 (-0.9) 1.55
  ] # layer FFab # width 0.1
  <> (
    pad 1 SMD Rect (V2 1.06 0.65) (newNet ref 1) # translate (V2 (-1.1) (-0.95))
    <> pad 2 SMD Rect (V2 1.06 0.65) (newNet ref 2) # translate (V2 (-1.1) 0)
    <> pad 3 SMD Rect (V2 1.06 0.65) (newNet ref 3) # translate (V2 (-1.1) 0.95)
    <> pad 4 SMD Rect (V2 1.06 0.65) (newNet ref 4) # translate (V2 1.1 0.95)
    <> pad 5 SMD Rect (V2 1.06 0.65) (newNet ref 5) # translate (V2 1.1 (-0.95))
  ) # layers [FCu, FPaste, FMask]

{-
    (pad 5 smd rect (at 1.1 -0.95) (size 1.06 0.65) (layers F.Cu F.Paste F.Mask))
    -}
