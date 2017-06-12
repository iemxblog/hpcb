module Kicad.Element
(
    --Module(..)
    V2(..)
    , Position(..)
    , Size(..)
) where

import Kicad.SExpr

newtype Layer = Layer String
data Net = Net Int String

data FpText = FpText
  String      -- ^ Name of the text
  String      -- ^ Content of the text
  Position    -- ^ Position of the text
  Layer       -- ^ Layer where the text is
  Visibility  -- ^ Visibility of the text
  Effects     -- ^ Effects (font, justification, etc.)

data Visibility = Visible | Hidden
data Effects = Effects Font [Justification]
data Justification = Mirror | Center | Left | Right | Top | Bottom
data Font = Font Size Thickness
newtype Thickness = Thickness Float

{-}
data Module = Module String Layer String String Position ModuleDescription
data ModuleDescription = ModuleDescription [Text] [Graphic] [Pad] Model
-}

data V2 a = V2 a a

-- | Position :
-- V2 Float : Coordinates
-- Maybe Float : Orientation
data Position = Position (V2 Float) Float

instance Itemizable Position where
    itemize (Position (V2 x y) 0.0)  = Item "position" [ParamFloat x, ParamFloat y]
    itemize (Position (V2 x y) o)  = Item "position" [ParamFloat x, ParamFloat y, ParamFloat o]

newtype Size = Size (V2 Float)

instance Itemizable Size where
    itemize (Size (V2 x y))  = Item "size" [ParamFloat x, ParamFloat y]
