module Main where

import Kicad.SExpr
import Kicad.Element

main :: IO ()
main = do
  print $ itemize (At (V2 3 4) Nothing)
  print $ itemize (FpText "reference" "R1" (At (V2 0 (-2.3)) (Just 45)) (Layer "F.Silks") StandardEffects)
