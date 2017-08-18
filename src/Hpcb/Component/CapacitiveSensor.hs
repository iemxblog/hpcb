module Hpcb.Component.CapacitiveSensor (
  capacitiveSensor
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Effects
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

-- | Circular capacitive sensor (a copper pad with solder mask on it, and a circle on the silkscreen)
capacitiveSensor :: String        -- ^ Reference
                    -> Float      -- ^ Diameter on copper layer
                    -> Float      -- ^ Diameter on silk screen
                    -> Circuit
capacitiveSensor ref diam1 diam2 = footprint ref "Capacitive_Sensor" $
  fpText "reference" ref defaultEffects # translate (V2 0 (-maxDiam/2-2)) # layer FSilkS
  <> fpText "value" val defaultEffects # translate (V2 0 (maxDiam/2+2)) # layer FFab
  <> fpCircle (diam2+0.5) # layer FSilkS # width 0.15
  <> fpCircle (maxDiam+1) # layer FCrtYd # width 0.05
  <> pad 1 SMD Circle (V2 diam1 diam1) (newNet ref 1) # layers [FCu]
  where
    val = "Capacitive_Sensor_" ++ show diam1 ++ "mm"
    maxDiam = max diam1 diam2
