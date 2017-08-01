 module Hpcb.Component.QFP (
  qfp_32,
  qfp_44
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.Layer
import Hpcb.Data.FpElement
import Hpcb.Functions
import Data.Monoid


qfp_32 :: String        -- ^ Reference
          -> String     -- ^ Value
          -> Circuit
qfp_32 ref val = footprint ref "TQFP-32" $
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
  <> mconcat [silkCorner # rotate (i*90) | i <- [0..2]]
  <> (
    fpLine (V2 (-3.625) (-3.625)) (V2 (-3.625) (-3.4))
    <> fpLine (V2 (-3.625) (-3.625)) (V2 (-3.3) (-3.625))
    <> fpLine (V2 (-3.625) (-3.4)) (V2 (-5.05) (-3.4))
  ) # layer FSilkS # width 0.15
  <> mconcat [padsLine (i*8+1) # rotate (90*fromIntegral i) | i <- [0..3]]
  where
    silkCorner = (
        fpLine (V2 (-3.625) 3.625) (V2 (-3.625) 3.3)
        <> fpLine (V2 (-3.625) 3.625) (V2 (-3.3) 3.625)
      ) # layer FSilkS # width 0.15
    padsLine nStart = mconcat [pad (nStart + i) SMD Rect (V2 1.6 0.55) (newNet ref (nStart + i)) # translate (V2 (-4.25) (-2.8+fromIntegral i*0.8)) | i <- [0..7]] # layers [FCu, FPaste, FMask]

qfp_44 :: String
          -> String
          -> Circuit
qfp_44 ref val = footprint ref "TQFP-44" $
  fpText "reference" ref StandardEffects # translate (V2 0 (-7.45)) # layer FSilkS
  <> fpText "value" val StandardEffects # translate (V2 0 7.45) # layer FFab
  <> fpText "user" ref StandardEffects # layer FFab
  <> fpPolygon [
    V2 (-5) (-4),
    V2 (-4) (-5),
    V2 5 (-5),
    V2 5 5,
    V2 (-5) 5
  ] # layer FFab # width 0.15
  <> fpRectangle 13.4 13.4 # layer FCrtYd # width 0.05
  <> mconcat [silkCorner # rotate (i*90) | i <- [0..2]]
  <> (
    fpLine (V2 (-5.175) (-5.175)) (V2 (-5.175) (-4.6))
    <> fpLine (V2 (-5.175) (-5.175)) (V2 (-4.5) (-5.175))
    <> fpLine (V2 (-5.175) (-4.6)) (V2 (-6.45) (-4.6))
  ) # layer FSilkS # width 0.15
  <> mconcat [padsLine (i*11+1) # rotate (90*fromIntegral i) | i <- [0..3]]
  where
    silkCorner = (
        fpLine (V2 (-5.175) 5.175) (V2 (-5.175) 4.5)
        <> fpLine (V2 (-5.175) 5.175) (V2 (-4.5) 5.175)
      ) # layer FSilkS # width 0.15
    padsLine nStart = mconcat [pad (nStart + i) SMD Rect (V2 1.5 0.55) (newNet ref (nStart + i)) # translate (V2 (-5.7) (-4+fromIntegral i*0.8)) | i <- [0..10]] # layers [FCu, FPaste, FMask]
