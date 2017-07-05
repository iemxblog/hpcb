module Main where

import Kicad.SExpr
import Kicad.Data
import Kicad.Circuit
import Data.Monoid

main :: IO ()
main = do
  --print $ itemize (At (V2 3 4) Nothing)
  --print $ itemize (FpText "reference" "R1" (At (V2 0 (-2.3)) (Just 45)) FSilkS StandardEffects)
  {-
  putStr $ prettyPrint $ itemize $
    Footprint "R3" FCu (TEdit "4E4C0E65") (TStamp "5127A136")
      (At (V2 66.04 33.3502) Nothing)
      [ FpText "reference" "R1" (At (V2 0 0.127) Nothing) FSilkS StandardEffects,
        FpText "value" "330k" (At (V2 0 0.127) Nothing) FSilkS StandardEffects,
        FpLine (V2 (-3.81) 0) (V2 (-3.302) 0) FSilkS 0.2032,
        Pad 1 ThroughHole Circle (At (V2 3.81 0) Nothing) (Size (V2 1.397 1.397)) (PadDrill 0.812799) (copperLayers ++ maskLayers ++ [FSilkS]) (Net 1 "/SIGNAL"),
        Pad 2 ThroughHole Circle (At (V2 3.81 0) Nothing) (Size (V2 1.397 1.397)) (PadDrill 0.812799) (copperLayers  ++ maskLayers ++ [FSilkS]) (Net 2 "GND")
      ]
      -}
  putStr $ prettyPrint $ itemize $
    translate (V2 66.04 33.3502) $
      layer FCu $
        footprint "R1" $
          translate (V2 0 0.127) $
            layer FSilkS (
              fpText "reference" "R1" StandardEffects
              <> fpText "value" "330k" StandardEffects
              <> fpLine (V2 (-3.81) 0) (V2 (-3.302) 0) 0.2032
            )
            <> translate (V2 3.81 0) (pad 1 ThroughHole Circle (Size (V2 1.397 1.397)) (PadDrill 0.812799) (Net 1 "/SIGNAL"))
            <> translate (V2 3.81 0) (pad 1 ThroughHole Circle (Size (V2 1.397 1.397)) (PadDrill 0.812799) (Net 2 "GND"))
