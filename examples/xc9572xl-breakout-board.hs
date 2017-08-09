module Main (
  main
) where

import Hpcb
import Data.Monoid

power :: Circuit
power = (
  lm1117 "U2"
  <> c805 "C1" "10uF-Tantalum" # rotate (-90) # translate (V2 (2.54*3) 0)
  <> c805 "C2" "10uF-Tantalum" # rotate 90 # translate (V2 (-2.54*3) 0)
  )
  # connect (net "RAW") (pinName "U2" "VIN")
  # connect (net "RAW") (pin "C1" 1)
  # connect (net "GND") (pin "C1" 2)
  # connect (net "GND") (pinName "U2" "GND")
  # connect (net "+3V3") (pinName "U2" "VOUT")
  # connect (net "+3V3") (pin "C2" 1)
  # connect (net "GND") (pin "C2" 2)

oscillator :: Circuit
oscillator =
  qx733A20 "Q1"
  # connect (net "+3V3") (pinName "Q1" "VCC")
  # connect (net "+3V3") (pinName "Q1" "EN")
  # connect (net "GND") (pinName "Q1" "GND")
  # connect (net "OSC") (pinName "Q1" "OUT")

jtag :: Circuit
jtag = pinHeaderFromNets "JTAG" [
  "TMS",
  "TDI",
  "TDO",
  "TCK",
  "GND",
  "+3.3V"
  ]

pinHeaderLeft :: Circuit
pinHeaderLeft =
  pinHeaderFromNets "JP1" netsList
  <> mconcat [text t # justify LeftJustify # translate (V2 (2.54*0.75) (fromIntegral (i::Int) * 2.54)) | (t, i) <- zip netsList [0..]]
    where
      netsList = [
        "IO1-2",
        "IO1-5",
        "IO1-6",
        "IO1-8",
        "IO1-9",
        "IO1-11",
        "IO1-14",
        "IO1-15",
        "IO1-17",
        "IO3-2",
        "IO3-5",
        "IO3-8",
        "IO3-9",
        "IO3-11",
        "IO3-14",
        "IO3-15",
        "IO3-16",
        "IO3-17",
        "GND"
        ]

pinHeaderRight :: Circuit
pinHeaderRight =
  pinHeaderFromNets "JP2" netsList
  <> mconcat [text t # justify RightJustify # translate (V2 (-2.54*0.75) (fromIntegral (i::Int) * 2.54)) | (t, i) <- zip netsList [0..]]
  where
    netsList = [
      "RAW",
      "GND",
      "+3V3",
      "IO2-17",
      "IO2-15",
      "IO2-14",
      "IO2-11",
      "IO2-9",
      "IO2-8",
      "IO2-6",
      "IO2-5",
      "IO2-2",
      "IO4-17",
      "IO4-15",
      "IO4-14",
      "IO4-11",
      "IO4-8",
      "IO4-5",
      "IO4-2"
      ]

cpld :: Circuit
cpld =
  xc9572xl "U1"
  # connect (net "GND") (pinName "U1" "GND")
  # connect (net "+3V3") (pinName "U1" "VCCIO")
  # connect (net "+3V3") (pinName "U1" "VCCINT")
  # connectNamesToNets "U1"[
    "IO1-14",
    "IO1-15",
    "IO1-17",
    "IO3-2",
    "IO3-5",
    "IO3-8",
    "IO3-9",
    "TDI",
    "TMS",
    "TCK",
    "IO3-11",
    "IO3-14",
    "IO3-15",
    "IO3-17",
    "IO3-16",
    "IO4-2",
    "IO4-5",
    "IO4-8",
    "IO4-11",
    "IO4-14",
    "TDO",
    "IO4-15",
    "IO4-17",
    "IO2-2",
    "IO2-5",
    "IO2-6",
    "IO2-8",
    "IO2-9",
    "IO2-11",
    "IO2-14",
    "IO2-15",
    "IO2-17",
    "IO1-2",
    "IO1-5",
    "IO1-6",
    "IO1-8",
    "IO1-9",
    "IO1-11"
  ]


xc9572xl_breakout_board :: Circuit
xc9572xl_breakout_board = (
    power # translate (V2 (6*2.54) (1*2.54))
    <> oscillator # translate (V2 (6*2.54) (4*2.54))
    <> jtag # rotate 90 # translate (V2 (4*2.54) (17*2.54))
    <> pinHeaderLeft
    <> pinHeaderRight # translate (V2 (12*2.54) 0)
    <> cpld # translate (V2 (6*2.54) (10*2.54))
  )
  # connect (pinName "Q1" "OUT") (pinName "U1" "GCK3")

main :: IO ()
main = runCircuit xc9572xl_breakout_board
