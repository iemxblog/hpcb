module Hpcb.Component.LED (
  led_805
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Connection
import Hpcb.Data.Effects
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

led_805 ::  String      -- ^ Reference
            -> String   -- ^ Value
            -> Circuit
led_805 ref val = (footprint ref "LED_0805" $
    fpText "reference" ref defaultEffects # translate (V2 0 (-1.45)) # layer FSilkS
    <> fpText "value" val defaultEffects # translate (V2 0 1.55) # layer FFab
    <> (
      fpLine (V2 (-1.8) (-0.7)) (V2 (-1.8) 0.7)
      <> fpLine (V2 (-1.8) 0.7) (V2 1 0.7)
      <> fpLine (V2 (-1.8) (-0.7)) (V2 1 (-0.7))
    ) # layer FSilkS # width 0.12
    <> (
      fpLine (V2 (-0.4) (-0.4)) (V2 (-0.4) 0.4)
      <> fpLine (V2 (-0.4) 0) (V2 0.2 (-0.4))
      <> fpLine (V2 0.2 0.4) (V2 (-0.4) 0)
      <> fpLine (V2 0.2 (-0.4)) (V2 0.2 0.4)
      <> fpLine (V2 1 0.6) (V2 (-1) 0.6)
      <> fpLine (V2 1 (-0.6)) (V2 1 0.6)
      <> fpLine (V2 (-1) (-0.6)) (V2 1 (-0.6))
      <> fpLine (V2 (-1) 0.6) (V2 (-1) (-0.6))
    ) # layer FFab # width 0.1
    <> fpRectangle 3.9 1.7 # layer FCrtYd # width 0.05
    <> (
      pad 1 SMD Rect (V2 1.2 1.2) (newNet ref 1) # translate (V2 1.1 0) # rotate 180
      <> pad 2 SMD Rect (V2 1.2 1.2) (newNet ref 2) # translate (V2 (-1.1) 0) # rotate 180
    ) # layers [FCu, FPaste, FMask]
  ) # names ref [(1, ["K"]), (2, ["A"])]
