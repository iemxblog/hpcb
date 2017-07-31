module Main (
  main
) where

import Hpcb
import Data.Monoid

power :: Circuit
power = (
  mic5205 "U2" # rotate 180 # translate (V2 0 (-12.8))
  <> c805 "C19" "10uF" # rotate 270 # translate (V2 3 (-12.8))
  <> led_805 "LED1" "Red" # rotate 90 # translate (V2 1.2 (-8.9))
  <> r805 "R11" "10k" # rotate 90 # translate (V2 (-1) (-9))
  <> c805 "C13" "10uF" # rotate 90 # translate (V2 (-3) (-12.8))
  <> c805 "C10" "0.1uF" # rotate 90 # translate (V2 (-5.1) (-12.8))
  )
  # connect (net "RAW") (pinName "U2" "IN")
  # connect (net "RAW") (pinName "U2" "EN")
  # connect (net "GND") (pinName "U2" "GND")
  # connect (net "RAW") (pin "C19" 1)
  # connect (net "GND") (pin "C19" 2)
  # connect (net "VCC") (pinName "U2" "OUT")
  # connect (net "VCC") (pinName "LED1" "A")
  # connect (pinName "LED1" "K") (pin "R11" 1)
  # connect (net "GND") (pin "R11" 2)
  # connect (net "VCC") (pin "C13" 1)
  # connect (net "GND") (pin "C13" 2)
  # connect (net "VCC") (pin "C10" 1)
  # connect (net "GND") (pin "C10" 2)

ftdi_basic :: String -> Circuit
ftdi_basic ref = pinHeader ref 6 1 # names ref [
  (1, ["DTR"]),
  (2, ["RXI"]),
  (3, ["TXO"]),
  (4, ["VCC"]),
  (5, ["CTS"]),
  (6, ["GND"])
  ]
  # connect (net "GND") (pinName ref "GND")
  # connect (net "GND") (pinName ref "CTS")
  # connect (net "VCC") (pinName ref "VCC")
  # connect (net "RXI") (pinName ref "TXO")
  # connect (net "TXO") (pinName ref "RXI")
  # connect (net "DTR") (pinName ref "DTR")

pinHeaderLeft :: String -> Circuit
pinHeaderLeft ref =
  pinHeader ref 1 12
  # connect (net "D9") (pin ref 1)
  # connect (net "D8") (pin ref 2)
  # connect (net "D7") (pin ref 3)
  # connect (net "D6") (pin ref 4)
  # connect (net "D5") (pin ref 5)
  # connect (net "D4") (pin ref 6)
  # connect (net "D3") (pin ref 7)
  # connect (net "D2") (pin ref 8)
  # connect (net "GND") (pin ref 9)
  # connect (net "RESET") (pin ref 10)
  # connect (net "RXI") (pin ref 11)
  # connect (net "TXO") (pin ref 12)

pinHeaderRight :: String -> Circuit
pinHeaderRight ref =
  pinHeader ref 1 12
  # connect (net "RAW") (pin ref 1)
  # connect (net "GND") (pin ref 2)
  # connect (net "RESET") (pin ref 3)
  # connect (net "VCC") (pin ref 4)
  # connect (net "A3") (pin ref 5)
  # connect (net "A2") (pin ref 6)
  # connect (net "A1") (pin ref 7)
  # connect (net "A0") (pin ref 8)
  # connect (net "SCK") (pin ref 9)
  # connect (net "MISO") (pin ref 10)
  # connect (net "MOSI") (pin ref 11)
  # connect (net "D10") (pin ref 12)

offGridHeader1 :: String -> Circuit
offGridHeader1 ref =
  pinHeader ref 1 2
  # connect (net "A4") (pin ref 1)
  # connect (net "A5") (pin ref 2)

offGridHeader2 :: String -> Circuit
offGridHeader2 ref =
  pinHeader ref 2 1
  # connect (net "A6") (pin ref 1)
  # connect (net "A7") (pin ref 2)


pinHeaders :: Circuit
pinHeaders =
  ftdi_basic "JP1" # translate (V2 (-6.35) (-15.24))
  <> pinHeaderLeft "JP7" # rotate 180 # translate (V2 (-7.62) 15.24)
  <> pinHeaderRight "JP6" # translate (V2 7.62 (-12.7))
  <> offGridHeader1 "JP2" # translate (V2 5.08 (-3.81))
  <> offGridHeader2 "JP3" # translate (V2 2.54 15.24)

mcu :: Circuit
mcu = (
  atmega328p_au "U1" # rotate (-45)
  <> r805 "R2" "10k"
  <> c805 "C2" "0.1uF"
  <> tact_switch "S1"
  <> c805 "C1" "0.1uF"
  -- <> resonator
  <> c805 "C3" "O.1uF"
  <> r805 "R1" ""
  <> r805 "R3" ""
  <> r805 "R6" "330"
  <> led_805 "D3" "Green"
  )
  # connect (net "DTR") (pin "C2" 1)
  # connect (net "RESET") (pin "C2" 2)
  # connect (net "VCC") (pin "R2" 1)
  # connect (net "RESET") (pin "R2" 2)
  # connect (net "RESET") (pin "S1" 1)
  # connect (net "GND") (pin "S1" 2)
  # connect (net "RESET") (pinName "U1" "RESET")
  # connect (net "AREF") (pinName "U1" "AREF")
  # connect (net "AREF") (pin "C1" 1)
  # connect (net "GND") (pin "C1" 2)
  -- # connect resonator
  # connect (net "VCC") (pin "C3" 1)
  # connect (net "GND") (pin "C3" 2)
  # connect (net "VCC") (pinName "U1" "VCC")
  # connect (net "VCC") (pinName "U1" "AVCC")
  # connect (net "GND") (pinName "U1" "GND")
  # connect (net "A0") (pinName "U1" "ADC0")
  # connect (net "A1") (pinName "U1" "ADC1")
  # connect (net "A2") (pinName "U1" "ADC2")
  # connect (net "A3") (pinName "U1" "ADC3")
  # connect (net "VCC") (pin "R1" 1)
  # connect (pinName "U1" "SDA") (pin "R1" 2)
  # connect (net "VCC") (pin "R3" 1)
  # connect (pinName "U1" "SCL") (pin "R3" 2)
  # connect (net "A4") (pinName "U1" "ADC4")
  # connect (net "A5") (pinName "U1" "ADC5")
  # connect (net "A6") (pinName "U1" "ADC6")
  # connect (net "A7") (pinName "U1" "ADC7")
  # connect (net "RXI") (pinName "U1" "RXD")
  # connect (net "TXO") (pinName "U1" "TXD")
  # connect (net "D2") (pinName "U1" "PD2")
  # connect (net "D3") (pinName "U1" "PD3")
  # connect (net "D4") (pinName "U1" "PD4")
  # connect (net "D5") (pinName "U1" "PD5")
  # connect (net "D6") (pinName "U1" "PD6")
  # connect (net "D7") (pinName "U1" "PD7")
  # connect (net "D8") (pinName "U1" "PB0")
  # connect (net "D9") (pinName "U1" "PB1")
  # connect (net "D10") (pinName "U1" "PB2")
  # connect (net "MOSI") (pinName "U1" "MOSI")
  # connect (net "MISO") (pinName "U1" "MISO")
  # connect (net "SCK") (pinName "U1" "SCK")
  # connect (net "SCK") (pin "R6" 1)
  # connect (pin "R6" 2) (pinName "D3" "A")
  # connect (net "GND") (pinName "D3" "K")


arduino_pro_mini :: Circuit
arduino_pro_mini =
  power
  <> mcu # translate (V2 0 5.08)
  <> rectangle 18 34 # layer EdgeCuts # width 0.15
  <> pinHeaders

main :: IO ()
main = runCircuit arduino_pro_mini
