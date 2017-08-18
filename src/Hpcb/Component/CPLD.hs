module Hpcb.Component.CPLD (
  xc9572xl
) where

import Hpcb.Component.QFP
import Hpcb.Data.Circuit
import Hpcb.Functions
import Hpcb.Data.Connection


-- | XC9572XL CPLD
xc9572xl :: String      -- ^ Reference
            -> Circuit
xc9572xl ref = qfp_44 ref "XC9572XL" # names ref [
  (1, ["IO1-14", "GCK3"]),
  (2, ["IO1-15"]),
  (3, ["IO1-17"]),
  (4, ["GND"]),
  (5, ["IO3-2"]),
  (6, ["IO3-5"]),
  (7, ["IO3-8"]),
  (8, ["IO3-9"]),
  (9, ["TDI"]),
  (10, ["TMS"]),
  (11, ["TCK"]),
  (12, ["IO3-11"]),
  (13, ["IO3-14"]),
  (14, ["IO3-15"]),
  (15, ["VCCINT"]),
  (16, ["IO3-17"]),
  (17, ["GND"]),
  (18, ["IO3-16"]),
  (19, ["IO4-2"]),
  (20, ["IO4-5"]),
  (21, ["IO4-8"]),
  (22, ["IO4-11"]),
  (23, ["IO4-14"]),
  (24, ["TDO"]),
  (25, ["GND"]),
  (26, ["VCCIO"]),
  (27, ["IO4-15"]),
  (28, ["IO4-17"]),
  (29, ["IO2-2"]),
  (30, ["IO2-5"]),
  (31, ["IO2-6"]),
  (32, ["IO2-8"]),
  (33, ["IO2-9", "GSR"]),
  (34, ["IO2-11", "GTS2"]),
  (35, ["VCCINT"]),
  (36, ["IO2-14", "GTS1"]),
  (37, ["IO2-15"]),
  (38, ["IO2-17"]),
  (39, ["IO1-2"]),
  (40, ["IO1-5"]),
  (41, ["IO1-6"]),
  (42, ["IO1-8"]),
  (43, ["IO1-9", "GCK1"]),
  (44, ["IO1-11", "GCK2"])
  ]
