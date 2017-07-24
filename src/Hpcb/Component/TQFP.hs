 module Hpcb.Component.TQFP (
  tqfp32
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.Layer
import Hpcb.Data.FpElement
import Hpcb.Functions
import Data.Monoid


tqfp32 :: String        -- ^ Reference
          -> String     -- ^ Value
          -> Circuit
tqfp32 ref val = footprint ref "TQFP-32" $
  fpText "reference" ref StandardEffects # translate (V2 0 (-6.05)) # layer FSilkS
  <> fpText "value" val StandardEffects # translate (V2 0 6.05) # layer FFab
  <> fpText "user" ref StandardEffects # layer FFab
  <> fpPolygon [
    V2 (-3.5) (-2.5),
    V2 (-2.5) (-3.5),
    V2 3.5 (-3.5),
    V2 3.5 3.5,
    V2 (-3.5) 3.5
  ] # layer FFab # width 0.15
  <> fpRectangle 10.6 10.6 # layer FCrtYd # width 0.05
  <> foldr (<>) mempty [silkCorner # rotate (i*90) | i <- [0..2]]
  <> (
    fpLine (V2 (-3.625) (-3.625)) (V2 (-3.625) (-3.4))
    <> fpLine (V2 (-3.625) (-3.625)) (V2 (-3.3) (-3.625))
    <> fpLine (V2 (-3.625) (-3.4)) (V2 (-5.05) (-3.4))
  ) # layer FSilkS # width 0.15
  <> foldr (<>) mempty [padsLine (i*8+1) # rotate (90*fromIntegral i) | i <- [0..3]]
  where
    silkCorner = (
        fpLine (V2 (-3.625) 3.625) (V2 (-3.625) 3.3)
        <> fpLine (V2 (-3.625) 3.625) (V2 (-3.3) 3.625)
      ) # layer FSilkS # width 0.15
    padsLine nStart = foldr (<>) mempty [pad (nStart + i) SMD Rect (V2 1.6 0.55) (newNet ref (nStart + i)) # translate (V2 (-4.25) (-2.8+fromIntegral i*0.8)) | i <- [0..7]] # layers [FCu, FPaste, FMask]
