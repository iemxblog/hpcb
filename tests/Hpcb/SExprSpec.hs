module Hpcb.SExprSpec (
    spec
) where

import Test.Hspec
import Hpcb.SExpr

spec :: SpecWith ()
spec =  describe "S-Expression" $ do
            it "shows correctly a basic S-Expression" $
                show (Item "at" [PFloat 3, PFloat 4]) `shouldBe` "(at 3.0 4.0)"

            describe "prettyPrint" $ do
              it "pretty-prints a non-indented S-Expression" $
                prettyPrint (Item "at" [PFloat 3, PFloat 4]) `shouldBe` "(at 3.0 4.0)\n"
              it "pretty-prints an indented S-Expression" $
                prettyPrint (
                  Item "module" [
                    PString "R3",
                    Item "layer" [PString "F.Cu"],
                    Item "tedit" [PString "4E4C0E65"],
                    Item "tstamp" [PString "5127A136"],
                    Item "at" [PFloat 66.04, PFloat 33.3502],
                    Item "fp_text" [
                      PString "reference",
                      PString "R1",
                      Item "at" [PFloat 0, PFloat 0.127],
                      Item "layer" [PString "F.SilkS"],
                      Item "effects" [
                        Item "font" [
                          Item "size" [PInt 1, PInt 1],
                          Item "thickness" [PFloat 0.15]
                        ]
                      ]
                    ]
                  ]
                ) `shouldBe` "(module \n" ++ idt 1 ++ "R3\n" ++ idt 1 ++ "(layer F.Cu)\n" ++ idt 1 ++ "(tedit 4E4C0E65)\n" ++ idt 1 ++ "(tstamp 5127A136)\n" ++ idt 1 ++ "(at 66.04 33.3502)\n" ++ idt 1 ++ "(fp_text \n" ++ idt 2 ++ "reference\n" ++ idt 2 ++ "R1\n" ++ idt 2 ++ "(at 0.0 0.127)\n" ++ idt 2 ++ "(layer F.SilkS)\n" ++ idt 2 ++ "(effects (font (size 1 1) (thickness 0.15)))\n" ++ idt 1 ++ ")\n)\n"
                where idt n = replicate (4*n) ' '
