module Main where

import Hpcb.SExpr
import Hpcb.Data.NetNumbering
import Hpcb.Component
import Hpcb.Data.Circuit
import Hpcb.Data.KicadPCB
import Data.Monoid

main :: IO ()
main =
  putStr $ prettyPrint $ kicadPCB $ numberNets $ Circuit [r805 "R1" "10k"] [] []
