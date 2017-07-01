module Kicad.Element.Base(
  V2(..),
  Position(..),
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
