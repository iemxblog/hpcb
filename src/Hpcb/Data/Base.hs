module Hpcb.Data.Base(
  V2(..),
  Position(..),
  applyMatrix,
  applyMatrixV2,
  origin,
  translation,
  rotation,
  reflectionX,
  reflectionY,
  TEdit(..),
  dummyTEdit,
  TStamp(..),
  dummyTStamp,
  defaultWidth
) where

import Hpcb.SExpr
import Data.Matrix

-- | 2 dimensional vector type
data V2 a = V2 a a deriving (Eq, Show)

-- | Position datatype, which keeps the coordinates of an object, and its orientation.
data Position = At {getX :: Float, getY :: Float, getO :: Float} deriving Show

-- | Transforms a 'Position' into a column vector of homogeneous coordinates.
fromPosition :: Position          -- ^ Position to be transformed
                -> Matrix Float
fromPosition (At x y o) = fromList 4 1 [x, y, o, 1]

-- | Transforms a column vector of homogeneous coordinates into a 'Position'
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

-- | Generates a translation matrix from a vector.
-- This function is used in class 'Transformable'.
translation ::  V2 Float          -- ^ Vector
                -> Matrix Float   -- ^ Translation matrix
translation (V2 x y) = fromList 4 4 [
  1, 0, 0, x,
  0, 1, 0, y,
  0, 0, 1, 0,
  0, 0, 0, 1
  ]

-- | Generates a rotation matrix from a vector.
-- This function is used in class 'Transformable'.
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

-- | Reflection matrix. The axis of reflexion is the X axis.
-- This matrix is used in class 'Transformable'.
reflectionX :: Matrix Float
reflectionX = fromList 4 4 [
  1, 0, 0, 0,
  0, -1, 0, 0,
  0, 0, -1, 0,
  0, 0, 0, 1
  ]

-- | Reflection matrix. The axis of reflection is the Y axis.
-- This matrix is used in class 'Transformable'.
reflectionY :: Matrix Float
reflectionY = fromList 4 4 [
  -1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, -1, 180,
  0, 0, 0, 1
  ]

instance Itemizable Position where
    itemize (At x y 0.0)  = Item "at" [PFloat x, PFloat y]
    itemize (At x y o)  = Item "at" [PFloat x, PFloat y, PFloat o]

newtype TEdit = TEdit String deriving Show
instance Itemizable TEdit where
  itemize (TEdit s)= Item "tedit" [PString s]

-- Dummy TEdit
dummyTEdit :: TEdit
dummyTEdit = TEdit "5893982A"

newtype TStamp = TStamp String deriving Show
instance Itemizable TStamp where
  itemize (TStamp s)= Item "tstamp" [PString s]

-- | Dummy time stamp
dummyTStamp :: TStamp
dummyTStamp = TStamp "5893982A"

-- | Width used for an object when no width is specified
defaultWidth :: Float
defaultWidth = 0.15
