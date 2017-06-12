module Kicad.Element
(
    --Module(..)
    V2(..)
    , Position(..)
    , Size(..)
) where

import Kicad.SExpr

data Layer = Layer String | Layers [String]

instance Itemizable Layer where
  itemize (Layer s) = Item "layer" [ParamString s]
  itemize (Layers xs) = Item "layers" $ map ParamString xs

data Net = Net Int String

data FpText = FpText
  String      -- ^ Name of the text
  String      -- ^ Content of the text
  Position    -- ^ Position of the text
  Layer       -- ^ Layer where the text is
  Effects     -- ^ Effects (font, justification, etc.)

instance Itemizable FpText where
    itemize (FpText name text pos layer effects) =
      Item "fptext" [
        ParamString name,
        ParamString text,
        ParamItem (itemize pos),
        ParamItem (itemize layer),
        ParamItem (itemize effects)
        ]

data Effects = StandardEffects

instance Itemizable Effects where
  itemize StandardEffects =
      Item "effects" [
        ParamItem (Item "font" [
          ParamItem (Item "size" [ParamInt 1, ParamInt 1]),
          ParamItem (Item "thickness" [ParamFloat 0.15])
        ] )
      ]

{-}
data Module = Module String Layer String String Position ModuleDescription
data ModuleDescription = ModuleDescription [Text] [Graphic] [Pad] Model
-}

data V2 a = V2 a a

-- | Position :
-- V2 Float : Coordinates
-- Maybe Float : Orientation
data Position = Position (V2 Float) (Maybe Float)

instance Itemizable Position where
    itemize (Position (V2 x y) (Just 0.0))  = Item "at" [ParamFloat x, ParamFloat y]
    itemize (Position (V2 x y) (Just o))  = Item "position" [ParamFloat x, ParamFloat y, ParamFloat o]
    itemize (Position (V2 x y) Nothing)  = Item "position" [ParamFloat x, ParamFloat y]

newtype Size = Size (V2 Float)

instance Itemizable Size where
    itemize (Size (V2 x y))  = Item "size" [ParamFloat x, ParamFloat y]
