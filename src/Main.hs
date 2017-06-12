module Main where

import Kicad.SExpr

main :: IO ()
main = do
  print $ Item "at" [ParamInt 2, ParamInt 3]
