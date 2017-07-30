module Hpcb.Component.Regulator (
  mic5205
) where

import Hpcb.Component.SOT
import Hpcb.Data.Connection
import Hpcb.Data.Circuit
import Hpcb.Functions

mic5205 :: String
          -> Circuit
mic5205 ref = sot_23_5 ref "MIC5205" # names ref [
    (1, ["IN"]),
    (2, ["GND"]),
    (3, ["EN"]),
    (4, ["BP"]),
    (5, ["OUT"])
  ]
