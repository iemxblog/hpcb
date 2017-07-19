module Hpcb.Data.Base(
  V2(..),
  Position(..),
  applyMatrix,
  applyMatrixV2,
  origin,
  translation,
  rotation,
  TEdit(..),
  dummyTEdit,
  TStamp(..),
  dummyTStamp,
  defaultWidth
) where

import Hpcb.SExpr
import Data.Matrix

data V2 a = V2 a a deriving (Eq, Show)

-- | Position :
-- V2 Float : Coordinates
-- Maybe Float : Orientation
data Position = At {getX :: Float, getY :: Float, getO :: Float} deriving Show

fromPosition :: Position -> Matrix Float
fromPosition (At x y o) = fromList 4 1 [x, y, o, 1]

toPosition :: Matrix Float -> Position
toPosition m = At x y o
  where [x, y, o, _] = toList m

applyMatrix :: Matrix Float -> Position -> Position
applyMatrix m p = toPosition $ multStd m (fromPosition p)

applyMatrixV2 :: Matrix Float -> V2 Float-> V2 Float
applyMatrixV2 m (V2 x y) = V2 x' y'
  where
    [x', y', _, _] = toList $multStd m (fromList 4 1 [x, y, 0, 1])

origin :: Position
origin = At 0 0 0

translation ::  V2 Float
                -> Matrix Float   -- ^ Translation matrix
translation (V2 x y) = fromList 4 4 [
  1, 0, 0, x,
  0, 1, 0, y,
  0, 0, 1, 0,
  0, 0, 0, 1
  ]

rotation :: Float             -- ^ Rotation angle
            -> Matrix Float   -- ^ Rotation matrix
rotation a = fromList 4 4 [
    cos (fromDegrees a), sin (fromDegrees a), 0, 0,
    -sin (fromDegrees a), cos (fromDegrees a), 0, 0,
    0, 0, 1, a,
    0, 0, 0, 1
  ]
  where
    fromDegrees deg = deg * pi / 180

instance Itemizable Position where
    itemize (At x y 0.0)  = Item "at" [PFloat x, PFloat y]
    itemize (At x y o)  = Item "at" [PFloat x, PFloat y, PFloat o]

newtype TEdit = TEdit String deriving Show
instance Itemizable TEdit where
  itemize (TEdit s)= Item "tedit" [PString s]

dummyTEdit :: TEdit
dummyTEdit = TEdit "5893982A"

newtype TStamp = TStamp String deriving Show
instance Itemizable TStamp where
  itemize (TStamp s)= Item "tstamp" [PString s]

dummyTStamp :: TStamp
dummyTStamp = TStamp "5893982A"

defaultWidth :: Float
defaultWidth = 0.15
