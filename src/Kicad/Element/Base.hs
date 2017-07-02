module Kicad.Element.Base(
  V2(..),
  Position(..),
  Transformation,
  translation,
  rotation,
  Size(..),
  TEdit(..),
  TStamp(..)
) where

import Kicad.SExpr

data V2 a = V2 a a

-- | Position :
-- V2 Float : Coordinates
-- Maybe Float : Orientation
data Position = At (V2 Float) (Maybe Float)

type Transformation = Position -> Position

translation :: V2 Float -> Transformation
translation (V2 x y) (At (V2 xp yp) a) = At (V2(xp+x) (yp+y)) a

rotation :: Float -> Transformation
rotation a (At (V2 x y) Nothing) = At (V2 x y) (Just a)
rotation a (At (V2 x y) (Just ma)) = At (V2 x y) (Just (ma + a))

instance Itemizable Position where
    itemize (At (V2 x y) (Just 0.0))  = Item "at" [PFloat x, PFloat y]
    itemize (At (V2 x y) (Just o))  = Item "at" [PFloat x, PFloat y, PFloat o]
    itemize (At (V2 x y) Nothing)  = Item "at" [PFloat x, PFloat y]

newtype Size = Size (V2 Float)

instance Itemizable Size where
    itemize (Size (V2 x y))  = Item "size" [PFloat x, PFloat y]

newtype TEdit = TEdit String
instance Itemizable TEdit where
  itemize (TEdit s)= Item "tedit" [PString s]

newtype TStamp = TStamp String
instance Itemizable TStamp where
  itemize (TStamp s)= Item "tstamp" [PString s]
