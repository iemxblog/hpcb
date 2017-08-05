module Hpcb.Component.PinHeader (
  pinHeader,
  pinHeaderFromNets
) where

import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Connection
import Hpcb.Data.Effects
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Layer
import Hpcb.Functions
import Data.Monoid

-- | Generates a vertical pin header from a list of nets.
-- Each pin will be connected to the corresponding net.
pinHeaderFromNets ::  String      -- ^ Reference
                      -> [String]    -- ^ ListOfNets
                      -> Circuit
pinHeaderFromNets ref xs = pinHeader ref 1 (length xs) # connectPinsToNets ref xs

-- | Generic pin header
pinHeader ::  String      -- ^ Reference
              -> Int      -- ^ Number of columns
              -> Int      -- ^ Number of rows
              -> Circuit
pinHeader ref cols rows = footprint ref headerDesc $
  fpText "reference" ref defaultEffects # translate (V2 0 (-2.54)) # layer FSilkS
  <> fpText "value" headerDesc defaultEffects # translate (V2 0 (fromIntegral rows * 2.54)) # layer FFab
  <> fpRectangle (fromIntegral (cols-1) * 2.54+2*1.75) (fromIntegral (rows-1) * 2.54+2*1.75) # translate (V2 (fromIntegral (cols-1) / 2 * 2.54) (fromIntegral (rows-1) / 2 * 2.54)) # layer FCrtYd # width 0.05
  <> case (cols, rows) of
        (0, _) -> mempty
        (_, 0) -> mempty
        (1, 1) -> singleHeaderSilk
        (1, rs) | rs > 1 -> verticalHeaderSilk rows
        (cs, 1) | cs > 1 -> horizontalHeaderSilk cols
        (_, _) -> rectangleHeaderSilk cols rows
     # layer FSilkS
  <> (pad 1 ThroughHole{getDrill=1.016} Rect firstPadSize (newNet ref 1)
  <> foldr (<>) mempty [pad (c+(r-1)*cols) ThroughHole{getDrill=1.016} Oval padsSize (newNet ref (c+(r-1)*cols)) # translate (V2 ((fromIntegral c - 1)*2.54) ((fromIntegral r - 1)*2.54)) | c <- [1..cols], r <- [1..rows], (c,r) /= (1,1)])
      # layers (copperLayers ++ maskLayers)
  where
    headerDesc = "CONN_" ++ min2 cols ++ "x" ++ min2 rows
    min2 x = if length (show x) < 2
              then "0" ++ show x
              else show x
    firstPadSize =
      case (cols, rows) of
        (1,1) -> V2 2.2352 2.2352
        (1,_) -> V2 2.032 1.7272
        (_,1) -> V2 1.7272 2.032
        (_, _) -> V2 1.7272 1.7272
    padsSize =
      case (cols, rows) of
        (1, _) -> V2 2.032 1.7272
        (_, 1) -> V2 1.7272 2.032
        (_, _) -> V2 1.7272 1.7272

-- | Generates the silk screen for a vertical pin header
verticalHeaderSilk :: Int           -- ^ Number of rows
                      -> FpContent
verticalHeaderSilk rows = (
  verticalCap
  <> fpPolygon [
      V2 (-1.27) 1.27,
      V2 1.27 1.27,
      V2 1.27 (2.54*(fromIntegral rows - 1)+1.27),
      V2 (-1.27) (2.54*(fromIntegral rows - 1)+1.27)
      ]
  ) # width 0.15

-- | Generates the silk screen for a horizontal pin header
horizontalHeaderSilk :: Int           -- ^ Number of columns
                        -> FpContent
horizontalHeaderSilk cols = (
  fpLine (V2 0 1.55) (V2 (-1.55) 1.55)
  <> fpLine (V2 (-1.55) 1.55) (V2 (-1.55) (-1.55))
  <> fpLine (V2 (-1.55) (-1.55)) (V2 0 (-1.55))
  <> fpPolygon [
      V2 1.27 (-1.27),
      V2 (2.54*(fromIntegral cols - 1) + 1.27) (-1.27),
      V2 (2.54*(fromIntegral cols - 1) + 1.27) 1.27,
      V2 1.27 1.27
      ]
  ) # width 0.15

-- | Generates the silk screen for a header whose number of columns and
-- rows are different of 1
rectangleHeaderSilk ::  Int           -- ^ Number of columns
                        -> Int        -- ^ Number of rows
                        -> FpContent
rectangleHeaderSilk cols rows = (
  fpLine (V2 (-1.55) (-1.55)) (V2 0 (-1.55))
  <>  fpLine (V2 (-1.55) (-1.55)) (V2 (-1.55) 0)
  <>  fpPolygon [
        V2 1.27 1.27,
        V2 1.27 (-1.27),
        V2 (2.54*(fromIntegral cols - 1) + 1.27) (-1.27),
        V2 (2.54*(fromIntegral cols - 1) + 1.27) (2.54*(fromIntegral rows - 1) + 1.27),
        V2 (-1.27) (2.54*(fromIntegral rows - 1) + 1.27),
        V2 (-1.27) 1.27
        ]
  ) # width 0.15

-- | Generates the silk screen for a single pin header
singleHeaderSilk :: FpContent
singleHeaderSilk = (
  verticalCap
  <> fpLine (V2 (-1.27) 1.27) (V2 1.27 1.27)
  ) # width 0.15

-- | Generates the cap on the silkscreen for single line headers
verticalCap :: FpContent
verticalCap =
  fpLine (V2 (-1.55) 0) (V2 (-1.55) (-1.55))
  <> fpLine (V2 (-1.55) (-1.55)) (V2 1.55 (-1.55))
  <> fpLine (V2 1.55 (-1.55)) (V2 1.55 0)
