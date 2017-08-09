module Main (
  main
) where

import Hpcb
import Data.Monoid

outline :: Circuit
outline =
  rectangle w h
  # translate (V2 (w/2-2.54) (h/2-2.54*1.5))
  # layer EdgeCuts
  where (w, h) = (4*2.54, 3.5*2.54)

ledBoard :: Circuit
ledBoard = (
  pinHeaderFromNets "JP1" ["VCC", "GND"]
  <> led_805 "D1" "RED" # rotate 180 # translate (V2 (2.54*2) (-1.27))
  <> r805 "R1" "330" # rotate 180 # translate (V2 (2.54*2) 2.54)
  <> outline
  )
  # connect (net "VCC") (pinName "D1" "A")
  # connect (pinName "D1" "K") (pin "R1" 1)
  # connect (net "GND") (pin "R1" 2)

main :: IO ()
main = runCircuit ledBoard
