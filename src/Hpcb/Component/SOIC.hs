module Hpcb.Component.SOIC (
 soic_8
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

-- | SOIC8 package
soic_8 :: String      -- ^ Reference
          -> String   -- ^ Value
          -> Circuit
soic_8 ref val = footprint ref "SOIC-8" $
  fpText "reference" ref defaultEffects # translate (V2 0 (-3.5)) # layer FSilkS
  <> fpText "value" val defaultEffects # translate (V2 0 3.5) # layer FFab
  <> fpPolygon [
    V2 (-1.95) (-1.45),
    V2 (-0.95) (-2.45),
    V2 1.95 (-2.45),
    V2 1.95 2.45,
    V2 (-1.95) 2.45
  ] # layer FFab # width 0.15
  <> fpRectangle 7.50 5.50 # layer FCrtYd # width 0.05
  <> (
    fpLine (V2 (-2.075) (-2.575)) (V2 (-2.075) (-2.525))
    <> fpLine (V2 2.075 (-2.575)) (V2 2.075 (-2.43))
    <> fpLine (V2 2.075 2.575) (V2 2.075 2.43)
    <> fpLine (V2 (-2.075) 2.575) (V2 (-2.075) 2.43)
    <> fpLine (V2 (-2.075) (-2.575)) (V2 2.075 (-2.575))
    <> fpLine (V2 (-2.075) 2.575) (V2 2.075 2.575)
    <> fpLine (V2 (-2.075) (-2.525)) (V2 (-3.475) (-2.525))
  ) # layer FSilkS # width 0.15
  <> (
    mconcat [pad (i+1) SMD Rect (V2 1.55 0.6) (newNet ref (i+1)) # translate (V2 (-2.7) (-1.905 + fromIntegral i * 1.27)) | i <- [0..3]]
    <> mconcat [pad (i+5) SMD Rect (V2 1.55 0.6) (newNet ref (i+5)) # translate (V2 2.7 (1.905 - fromIntegral i * 1.27)) | i <- [0..3]]
  ) # layers [FCu, FPaste, FMask]
