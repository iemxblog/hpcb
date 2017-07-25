 module Hpcb.Component.TQFP (
  tqfp_32,
  tqfp_44
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.Layer
import Hpcb.Data.FpElement
import Hpcb.Functions
import Data.Monoid


tqfp_32 :: String        -- ^ Reference
          -> String     -- ^ Value
          -> Circuit
tqfp_32 ref val = footprint ref "TQFP-32" $
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

tqfp_44 :: String
          -> String
          -> Circuit
tqfp_44 ref val = footprint ref "TQFP-44" $
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


{-
(pad 1 smd rect (at -5.7 -4) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 2 smd rect (at -5.7 -3.2) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 3 smd rect (at -5.7 -2.4) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 4 smd rect (at -5.7 -1.6) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 5 smd rect (at -5.7 -0.8) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 6 smd rect (at -5.7 0) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 7 smd rect (at -5.7 0.8) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 8 smd rect (at -5.7 1.6) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 9 smd rect (at -5.7 2.4) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 10 smd rect (at -5.7 3.2) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 11 smd rect (at -5.7 4) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 12 smd rect (at -4 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 13 smd rect (at -3.2 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 14 smd rect (at -2.4 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 15 smd rect (at -1.6 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 16 smd rect (at -0.8 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 17 smd rect (at 0 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 18 smd rect (at 0.8 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 19 smd rect (at 1.6 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 20 smd rect (at 2.4 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 21 smd rect (at 3.2 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 22 smd rect (at 4 5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 23 smd rect (at 5.7 4) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 24 smd rect (at 5.7 3.2) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 25 smd rect (at 5.7 2.4) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 26 smd rect (at 5.7 1.6) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 27 smd rect (at 5.7 0.8) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 28 smd rect (at 5.7 0) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 29 smd rect (at 5.7 -0.8) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 30 smd rect (at 5.7 -1.6) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 31 smd rect (at 5.7 -2.4) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 32 smd rect (at 5.7 -3.2) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 33 smd rect (at 5.7 -4) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 34 smd rect (at 4 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 35 smd rect (at 3.2 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 36 smd rect (at 2.4 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 37 smd rect (at 1.6 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 38 smd rect (at 0.8 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 39 smd rect (at 0 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 40 smd rect (at -0.8 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 41 smd rect (at -1.6 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 42 smd rect (at -2.4 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 43 smd rect (at -3.2 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))
(pad 44 smd rect (at -4 -5.7 90) (size 1.5 0.55) (layers F.Cu F.Paste F.Mask))

-}
