# HPCB
Create a PCB programmatically instead of using a GUI
(like OpenSCAD for 3d modelling)

# Hello world !

Install Haskell :

~~~~~
$ sudo apt install haskell-platform
~~~~~

Create a directory with a sandbox :

~~~~~
$ mkdir hello-world
$ cd hello-world
$ cabal sandbox init
$ cabal install hpcb
~~~~~

Edit led.hs and copy/paste the following code :

~~~~~
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
~~~~~

Then produce the board file like this :

~~~~~
ghc --make led.hs
./led > led.kicad_pcb
~~~~~

Then open led.kicad_pcb with Kicad.

![led](led.jpg "Led example")


# What does it do ?
- Places components, translates, rotates them (and composes translations and rotations)
- Makes electrical connections (ratsnest, but not routing)
- Outputs a Kicad PCB file

# What doesn't it do (yet) ?
- Routing
- Flipping components (putting them on the back side of a board)

# How to
## Connect components
4 functions are available to make connections :
connect, net, pin, and pinName.

Here are some examples :

### Example 1
Create a resistor named "R1", of value "10k" and connect its pin number 1 to net "GND".

~~~~~
resistor = r805 "R1" "10k" # connect (net "GND") (pin "R1" 1)
~~~~~

### Example 2
Create an LED named "D1", of value "RED", and connect its cathode named "K" to net "GND"

~~~~~
led = led_805 "D1" "RED" # connect (net "GND") (pinName "D1" "K")
~~~~~

### Example 3
Create an Atemaga328p named "U1", and connect all its VCC pins to net "VCC", and all its GND pins to net "GND".

~~~~~
mcu =
  atmega328p_au "U1"
  # connect (net "VCC") (pinName "VCC")
  # connect (net "VCC") (pinName "AVCC")
  # connect (net "GND") (pinName "U1" "GND")
~~~~~

### Example 4
Create 2 leds, an connect their 2 anodes together.

~~~~~
leds =
  ( led_805 "D1" "RED"
  <> led_805 "D2" "GREEN"
  )
  # connect (pinName "D1" "A") (pinName "D2" "A")

~~~~~

### Pitfalls

#### Out of scope components

~~~~~
leds =
  led_805 "D1" "RED"
  <> led_805 "D2" "GREEN" # connect (pinName "D1" "A") (pinName "D2" "A")
~~~~~

This example will return an error saying that it cannot find component D1. It is normal because operator # has a higher precedence than <>,
so "pinName" looks only in the circuit returned by this function :

~~~~~
led_805 "D2" "GREEN"
~~~~~

And there is no component named "D1".

#### Overwriting Nets

In this circuit, pin 1 of resistor R1 won't be connected to pin "SDA" of the atmega328p_au. The reason is that the SDA net in component R1 is out of scope when we make the connection in component U1.

~~~~~
mcu =
  atmega328p_au "U1"
  # connect (net "SDA") (pinName "U1" "ADC4")
  # connect (net "ADC4") (pinName "U1" "ADC4")


r =
  r805 "R1" "1k"
  # connect (net "VCC") (pinName "R1" 1)
  # connect (net "SDA") (pinName "R1" 2)

circuit = mcu <> r
~~~~~

The correct way would be to do like this :

~~~~~
circuit = (
  atmega328p_au "U1"
  r805 "R1" "1k"
  )
  # connect (net "SDA") (pinName "U1" "ADC4")
  # connect (net "ADC4") (pinName "U1" "ADC4")
  # connect (net "VCC") (pinName "R1" 1)
  # connect (net "SDA") (pinName "R1" 2)
~~~~~




# For contributors

## TODO
Documentation
Tests
New footprints
Flipping components
Routing

## Coordinate system

In the Kicad file format, a footprint has a location and an orientation. So when we apply a transformation to a component, we have to keep track of its orientation.

The coordinate system used in Hpcb is inpired by homogeneous coordinates. Here are the coordinates of some footprint (a column vector) :

~~~~~
(x)
(y)
(o)
(1)
~~~~~

"x" and "y" are the coordinates of the footprint. "o" is its orientation. Here is how we make a translation :

~~~~~
(x')      (1  0   0   tx)  (x)
(y')  =   (0  1   0   ty)  (y)
(o')      (0  0   1   0 )  (o)
(1 )      (0  0   0   1 )  (1)
~~~~~

Here is how we make a rotation :

~~~~~
(x')      (cos a  sin a   0   0)  (x)
(y')  =   (-sin a cos a   0   0)  (y)
(o')      (0      0       1   a)  (o)
(1 )      (0      0       0   1)  (1)
~~~~~


## How to make a new footprint

## Instantiate a new component from an existing footprint

# Links

## Interesting projects made by other people

### Skidl

[SKiDL](https://github.com/xesscorp/skidl) is a module that extends Python with the ability to design electronic circuits.

### PCBmodE

[PCBmodE](pcbmode.com) is a printed circuit board design Python script that creates an SVG from JSON input files, and then creates Gerber and Excellon files for manufacturing.

## Kicad file format description

[Kicad file format](http://kicad-pcb.org/help/file-formats/)
