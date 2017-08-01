module Hpcb.Component.Oscillator (
  qx733A20
) where

import Hpcb.Data.Circuit
import Hpcb.Functions
import Hpcb.Data.Effects
import Hpcb.Data.Action
import Hpcb.Data.FpElement
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Connection
import Data.Monoid

-- | QX733A20.00000B15M (farnell reference : 2508769)
qx733A20 :: String
            -> Circuit
qx733A20 ref = (footprint ref "QX733A20.00000B15M" $
    fpText "reference" ref StandardEffects # translate (V2 0 (-4.2)) # layer FSilkS
    <> fpText "value" "QX733A20.00000B15M" StandardEffects # translate (V2 0 4.2) # layer FFab
    <> fpRectangle 7 5 # layer FFab # width 0.1
    <> fpRectangle 7.5 6.7 # layer FCrtYd # width 0.05
    <> (
      pad 1 SMD Rect (V2 1.8 2.0) (newNet ref 1) # translate (V2 (-5.08/2) (4.20/2))
      <> pad 2 SMD Rect (V2 1.8 2.0) (newNet ref 2) # translate (V2 (5.08/2) (4.20/2))
      <> pad 3 SMD Rect (V2 1.8 2.0) (newNet ref 3) # translate (V2 (5.08/2) (-4.20/2))
      <> pad 4 SMD Rect (V2 1.8 2.0) (newNet ref 4) # translate (V2 (-5.08/2) (-4.20/2))
    ) # layers [FCu, FPaste, FMask]
  ) # names ref [
    (1, ["EN"]),
    (2, ["GND"]),
    (3, ["OUT"]),
    (4, ["VCC"])
  ]
