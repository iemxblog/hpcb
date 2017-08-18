module Hpcb.Component.SOT (
  sot_23,
  sot_23_5,
  sot_223,
  sot_223_3
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

-- | SOT-23 package
sot_23 ::  String      -- ^ Reference
          -> String   -- ^ Value
          -> Circuit
sot_23 ref val = footprint ref "SOT-23" $
  fpText "reference" ref defaultEffects # translate (V2 0 (-2.5)) # layer FSilkS
  <> fpText "value" val defaultEffects # translate (V2 0 2.5) # layer FFab
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

-- | SOT-23-5 package
sot_23_5 ::  String      -- ^ Reference
            -> String   -- ^ Value
            -> Circuit
sot_23_5 ref val = footprint ref "SOT-23-5" $
  fpText "reference" ref defaultEffects # translate (V2 0 (-2.9)) # layer FSilkS
  <> fpText "value" val defaultEffects # translate (V2 0 2.9) # layer FFab
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


sot_223_n ::  Int         --  Number of pins (3 or 4)
              -> String   -- ^ Reference
              -> String   -- ^ Value
              -> Circuit
sot_223_n n ref val = footprint ref "SOT-223" $
  fpText "reference" ref defaultEffects # translate (V2 0 (-4.5)) # layer FSilkS
  <> fpText "value" val defaultEffects # translate (V2 0 4.5) # layer FFab
  <> fpRectangle 8.8 7.2 # layer FCrtYd # width 0.05
  <> (
    fpLine (V2 1.91 3.41) (V2 1.91 2.15)
    <> fpLine (V2 1.91 (-3.41)) (V2 1.91 (-2.15))
    <> fpLine (V2 (-1.85) 3.41) (V2 1.91 3.41)
    <> fpLine (V2 (-4.1) (-3.41)) (V2 1.91 (-3.41))
  ) # layer FSilkS # width 0.12
  <> fpPolygon [
    V2 (-1.85) (-2.3),
    V2 (-0.8) (-3.35),
    V2 1.85 (-3.35),
    V2 1.85 3.35,
    V2 (-1.85) 3.35
  ] # layer FFab # width 0.1
  <> (
    pad 1 SMD Rect (V2 2 1.5) (newNet ref 1) # translate (V2 (-3.15) (-2.3))
    <> pad 2 SMD Rect (V2 2 1.5) (newNet ref 2) # translate (V2 (-3.15) 0)
    <> pad 3 SMD Rect (V2 2 1.5) (newNet ref 3) # translate (V2 (-3.15) 2.3)
    <> (if n == 3 then
          pad 2 SMD Rect (V2 2 3.8) (newNet ref 2) # translate (V2 3.15 0)
        else
          pad 4 SMD Rect (V2 2 3.8) (newNet ref 4) # translate (V2 3.15 0)
    )
  ) # layers [FCu, FPaste, FMask]

-- |  SOT-223 package
sot_223 ::  String
            -> String
            -> Circuit
sot_223 = sot_223_n 4

-- | SOT-223-3 package
sot_223_3 :: String
            -> String
            -> Circuit
sot_223_3 = sot_223_n 3
