module Kicad.Element
(
    --Module(..)
    V2(..)
    , Layer(..)
    , FpText(..)
    , Effects(..)
    , At(..)
    , Size(..)
) where

import Kicad.SExpr

data Layer = Layer String | Layers [String]

instance Itemizable Layer where
  itemize (Layer s) = Item "layer" [PString s]
  itemize (Layers xs) = Item "layers" $ map PString xs

data Net = Net Int String

data FpText = FpText
  String      -- ^ Name of the text
  String      -- ^ Content of the text
  At          -- ^ Position of the text
  Layer       -- ^ Layer where the text is
  Effects     -- ^ Effects (font, justification, etc.)

instance Itemizable FpText where
    itemize (FpText name text pos layer effects) =
      Item "fptext" [
        PString name,
        PString text,
        itemize pos,
        itemize layer,
        itemize effects
        ]

data Effects = StandardEffects

instance Itemizable Effects where
  itemize StandardEffects =
      Item "effects" [
        Item "font" [
          Item "size" [PInt 1, PInt 1],
          Item "thickness" [PFloat 0.15]
        ]
      ]

{-}
data Module = Module String Layer String String Position ModuleDescription
data ModuleDescription = ModuleDescription [Text] [Graphic] [Pad] Model
-}

data V2 a = V2 a a

-- | Position :
-- V2 Float : Coordinates
-- Maybe Float : Orientation
data At = At (V2 Float) (Maybe Float)

instance Itemizable At where
    itemize (At (V2 x y) (Just 0.0))  = Item "at" [PFloat x, PFloat y]
    itemize (At (V2 x y) (Just o))  = Item "at" [PFloat x, PFloat y, PFloat o]
    itemize (At (V2 x y) Nothing)  = Item "at" [PFloat x, PFloat y]

newtype Size = Size (V2 Float)

instance Itemizable Size where
    itemize (Size (V2 x y))  = Item "size" [PFloat x, PFloat y]
