module Kicad.Element
(
    --Module(..)
    V2(..)
    , Position(..)
    , Size(..)
) where

import Kicad.SExpr

{-}
data Module = Module String Layer String String Position ModuleDescription
data ModuleDescription = ModuleDescription [Text] [Graphic] [Pad] Model
-}

data V2 a = V2 a a
newtype Position = Position (V2 Float)

instance Itemizable Position where
    itemize (Position (V2 x y))  = Item "position" [(ParamFloat x), (ParamFloat y)]

newtype Size = Size (V2 Float)

instance Itemizable Size where
    itemize (Size (V2 x y))  = Item "size" [(ParamFloat x), (ParamFloat y)]
