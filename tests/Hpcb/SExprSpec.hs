module Hpcb.SExprSpec (
    spec
) where

import Test.Hspec
import Hpcb.SExpr

spec =  describe "S-Expression" $ do
            it "shows correctly a basic S-Expression" $
                show (Item "at" [PFloat 3, PFloat 4]) `shouldBe` "(at 3.0 4.0)"
