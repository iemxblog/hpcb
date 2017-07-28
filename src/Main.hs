module Main where

import Hpcb
import Data.Monoid


main :: IO ()
main = runCircuit $
  ( r805 "R1" "10k" # translate (V2 10 0) # rotate 45
    <> r805 "R2" "10k" # translate (V2 10 0) # rotate 30
    <> pinHeader "P1" 4 6
    <> capacitiveSensor "B1" 5 6 # translate (V2 (-10) 0)
    <> rectangle 50 30 # layer EdgeCuts # width 0.15
  )
  # connect (net "GND") (pin "P1" 5)
  # connect (net "GND") (pin "R1" 1)
  <> mconcat [r805 ("R" ++ show (i+3)) "10k" # translate (V2 15 0) # rotate (360/6*fromIntegral i) | i <- [0..5] ]
  # connect (pin "R3" 1) (pin "R4" 1)
  <> sot_23 "D1" "ZENER_3.3V" # translate (V2 20 0)
  <> sot_23_5 "U1" "MIC5205" # translate (V2 20 10)
  <> atmega328p_au "U2" # translate (V2 (-20) 10)
    # connect (net "GND") (pinName "U2" "GND")
  <> tqfp_44 "U3" "ATMEGA32U4" # translate (V2 (-17) (-7))
  <> lm358n "U4" # translate (V2 17.4 (-9.7))
  <> led_805 "D2" # translate (V2 13.5 5.9)
