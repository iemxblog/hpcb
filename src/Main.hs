module Main where

import Hpcb.SExpr
import Hpcb.Data.NetNumbering
import Hpcb.Component
import Hpcb.Data.Circuit
import Hpcb.Data.KicadPCB
import Hpcb.Data.Action
import Hpcb.Functions
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Data.Monoid

main :: IO ()
main =
  putStr $ prettyPrint $ kicadPCB $ numberNets $
    Circuit [
      r805 "R1" "10k" # translate (V2 10 0) # rotate 45,
      pinHeader "P1" "CONN_02x03" 4 6

      ] [] []
    <> rectangle 50 30 0.15 # layer EdgeCuts
