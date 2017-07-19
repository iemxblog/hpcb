module Main where

import Hpcb.SExpr
import Hpcb.Data.NetNumbering
import Hpcb.Component
import Hpcb.Data.KicadPCB
import Hpcb.Data.Action
import Hpcb.Functions
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Data.Monoid

main :: IO ()
main =
  putStr $ prettyPrint $ kicadPCB $ numberNets $
    r805 "R1" "10k" # translate (V2 10 0) # rotate 45
    <> r805 "R1" "10k" # translate (V2 10 0) # rotate 30
    <> pinHeader "P1" 4 6
    <> capacitiveSensor "B1" 5 6 # translate (V2 (-10) 0)
    <> rectangle 50 30 # layer EdgeCuts # width 0.15
