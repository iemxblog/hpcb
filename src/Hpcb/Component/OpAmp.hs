module Hpcb.Component.OpAmp (
  lm358n
) where

import Hpcb.Component.SOIC
import Hpcb.Data.Connection
import Hpcb.Data.Circuit
import Hpcb.Functions

lm358n :: String
          -> Circuit
lm358n ref = soic_8 ref "LM358N" # names ref [
    (1, ["OUTA"]),
    (2, ["-INA"]),
    (3, ["+INA"]),
    (4, ["GND", "V-"]),
    (5, ["+INB"]),
    (6, ["-INB"]),
    (7, ["OUTB"]),
    (8, ["V+"])
  ]
