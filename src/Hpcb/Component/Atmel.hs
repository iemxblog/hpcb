module Hpcb.Component.Atmel (
  atmega328p_au
) where

import Hpcb.Component.TQFP
import Hpcb.Data.Connection
import Hpcb.Data.Circuit
import Hpcb.Functions

atmega328p_au ::  String      -- ^ Reference
                  -> Circuit
atmega328p_au ref =
  tqfp_32 ref "ATMEGA328P-AU" # names ref [
    (1, "PD3"),
    (2, "PD4"),
    (3, "GND"),
    (4, "VCC"),
    (5, "GND"),
    (6, "VCC"),
    (7, "PB6"),
    (8, "PB7"),
    (9, "PD5"),
    (10, "PD6"),
    (11, "PD7"),
    (12, "PB0"),
    (13, "PB1"),
    (14, "PB2"),
    (15, "PB3"),
    (16, "PB4"),
    (17, "PB5"),
    (18, "AVCC"),
    (19, "ADC6"),
    (20, "AREF"),
    (21, "GND"),
    (22, "ADC7"),
    (23, "PC0"),
    (24, "PC1"),
    (25, "PC2"),
    (26, "PC3"),
    (27, "PC4"),
    (28, "PC5"),
    (29, "PC6"),
    (30, "PD0"),
    (31, "PD1"),
    (32, "PD2")
    ]
