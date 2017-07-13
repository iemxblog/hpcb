module Hpcb.NetNumberingSpec (
    spec
) where

import Test.Hspec
import Hpcb.NetNumbering
import Hpcb.Data.FpElement
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Effects
import Hpcb.Data.Net
import Hpcb.Data.Footprint
import Hpcb.Data.Circuit

spec :: SpecWith ()
spec = describe "Net numbering" $ do
          describe "listOfNets" $ do
            it "returns the correct list of nets for a basic list of FpElement" $
              listOfNets (Circuit [Footprint "R1" FCu dummyTEdit dummyTStamp origin (FpContent [
                FpLine (V2 1 2) (V2 3 4) FCu 1,
                FpCircle (V2 1 2) (V2 3 4) BCu 1,
                FpText "ref" "r1" (At (V2 1 2) Nothing) FCu StandardEffects,
                Pad 1 SMD Rect (At (V2 3 4) Nothing) (V2 1 2) 0.5 [FCu, FPaste, FMask] (Net "/SIGNAL"),
                Pad 2 SMD Rect (At (V2 3 4) Nothing) (V2 1 2) 0.5 [FCu, FPaste, FMask] (Net "GND"),
                Pad 3 SMD Rect (At (V2 3 4) Nothing) (V2 1 2) 0.5 [FCu, FPaste, FMask] (Net "VCC")
              ])] [] []) `shouldBe` [Net "/SIGNAL", Net"GND", Net "VCC"]
