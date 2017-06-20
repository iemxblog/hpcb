module Main where

import Kicad.SExpr
import Kicad.Element

main :: IO ()
main = do
  print $ itemize (At (V2 3 4) Nothing)
  print $ itemize (FpText "reference" "R1" (At (V2 0 (-2.3)) (Just 45)) (Layer "F.Silks") StandardEffects)
  print $ itemize $
    Module "R3" (Layer "top_side.Cu") (TEdit "4E4C0E65") (TStamp "5127A136")
      (At (V2 66.04 33.3502) Nothing)
      [ FpText "reference" "R1" (At (V2 0 0.127) Nothing) (Layer "F.SilkS") StandardEffects,
        FpText "value" "330k" (At (V2 0 0.127) Nothing) (Layer "F.SilkS") StandardEffects]
      [ FpLine (V2 (-3.81) 0) (V2 (-3.302) 0) (Layer "F.SilkS") 0.2032]
      [ Pad 1 ThroughHole Circle (At (V2 3.81 0) Nothing) (Size (V2 1.397 1.397)) (PadDrill 0.812799) (Layers ["*.Cu", "*.Mask", "F.SilkS"]) (Net 1 "/SIGNAL"),
        Pad 2 ThroughHole Circle (At (V2 3.81 0) Nothing) (Size (V2 1.397 1.397)) (PadDrill 0.812799) (Layers ["*.Cu", "*.Mask", "F.SilkS"]) (Net 2 "GND")
      ]
