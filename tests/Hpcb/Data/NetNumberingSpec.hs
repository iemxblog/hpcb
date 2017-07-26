module Hpcb.Data.NetNumberingSpec (
    spec
) where

import Test.Hspec
import Hpcb.Data.NetNumbering
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
              listOfNets (Circuit [Footprint "U1" "R_805" FCu dummyTEdit dummyTStamp origin (FpContent [
                FpLine (V2 1 2) (V2 3 4) FCu 1,
                FpCircle (V2 1 2) (V2 3 4) BCu 1,
                FpText "ref" "r1" (At 1 2 0) FCu StandardEffects,
                Pad 1 "" SMD Rect (At 3 4 0) (V2 1 2) [FCu, FPaste, FMask] (Net "/SIGNAL"),
                Pad 2 "" SMD Rect (At 3 4 0) (V2 1 2) [FCu, FPaste, FMask] (Net "GND"),
                Pad 3 "" SMD Rect (At 3 4 0) (V2 1 2) [FCu, FPaste, FMask] (Net "VCC")
              ])] [] []) `shouldBe` [Net "/SIGNAL", Net"GND", Net "VCC"]
