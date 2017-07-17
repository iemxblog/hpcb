module Hpcb.Component.PinHeader (
  pinHeader
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Effects
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Data.Net
import Hpcb.Functions
import Data.Monoid

-- | SMD Resistor, 805 package (2012 metric)
pinHeader ::  String      -- ^ Reference
              -> Int      -- ^ Number of columns
              -> Int      -- ^ Number of rows
              -> Footprint
pinHeader ref cols rows = footprint headerDesc $
  fpText "reference" ref StandardEffects # translate (V2 0 (-5.1)) # layer FSilkS
  <> fpText "value" headerDesc StandardEffects # translate (V2 0 (-3.1)) # layer FFab
  <> (  fpLine (V2 (-1.55) (-1.55)) (V2 0 (-1.55)) 0.15
        <>  fpLine (V2 (-1.55) (-1.55)) (V2 (-1.55) 0) 0.15
        <>  fpPolygon 0.15 [
          V2 1.27 1.27,
          V2 1.27 (-1.27),
          V2 (2.54*(fromIntegral cols - 1) + 1.27) (-1.27),
          V2 (2.54*(fromIntegral cols - 1) + 1.27) (2.54*(fromIntegral rows - 1) + 1.27),
          V2 (-1.27) (2.54*(fromIntegral rows - 1) + 1.27),
          V2 (-1.27) 1.27
      ]
    ) # layer FSilkS
  <> fpRectangle (fromIntegral (cols-1) * 2.54+2*1.75) (fromIntegral (rows-1) * 2.54+2*1.75) 0.05 # translate (V2 (fromIntegral (cols-1) / 2 * 2.54) (fromIntegral (rows-1) / 2 * 2.54)) # layer FCrtYd
  <> (pad 1 ThroughHole{getDrill=1.016} Rect (V2 1.7272 1.7272) (newNet ref 1)
  <> foldr (<>) mempty [pad (c+(r-1)*cols) ThroughHole{getDrill=1.016} Oval (V2 1.7272 1.7272) (newNet ref (c+(r-1)*cols)) # translate (V2 ((fromIntegral c - 1)*2.54) ((fromIntegral r - 1)*2.54)) | c <- [1..cols], r <- [1..rows], (c,r) /= (1,1)])
      # layers (copperLayers ++ maskLayers)
  where
    headerDesc = "CONN_" ++ min2 cols ++ "x" ++ min2 rows
    min2 x = if length (show x) < 2
              then "0" ++ show x
              else show x
