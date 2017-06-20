module Kicad.Element
(
    --Module(..)
    V2(..)
    , Layer(..)
    , Net(..)
    , FpText(..)
    , Effects(..)
    , Module(..)
    , TEdit(..)
    , TStamp(..)
    , Position(..)
    , Size(..)
    , Pad(..)
    , PadType(..)
    , PadShape(..)
    , PadDrill(..)
    , Graphic(..)
) where

import Kicad.SExpr

data Layer = Layer String | Layers [String]

instance Itemizable Layer where
  itemize (Layer s) = Item "layer" [PString s]
  itemize (Layers xs) = Item "layers" $ map PString xs

data Net = Net Int String
instance Itemizable Net where
  itemize (Net n s) = Item "net" [PInt n, PString s]

data FpText = FpText
  String      -- ^ Name of the text
  String      -- ^ Content of the text
  Position    -- ^ Position of the text
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

-- | Effects are not implemented yet
-- There is just a single constructor to provide one effect
data Effects = StandardEffects

instance Itemizable Effects where
  itemize StandardEffects =
      Item "effects" [
        Item "font" [
          Item "size" [PInt 1, PInt 1],
          Item "thickness" [PFloat 0.15]
        ]
      ]


data Module = Module
  String    -- ^ Module Name
  Layer     -- ^ Module layer
  TEdit     -- ^ Last edition time stamp
  TStamp    -- ^ Time stamp from the schematic
  Position  -- ^ Module position
  [FpText]
  [Graphic]
  [Pad]


instance Itemizable Module where
  itemize (Module n l te ts pos texts graphics pads) =
    Item "Module" ([PString n, itemize l, itemize te, itemize ts, itemize pos] ++ map itemize texts ++ map itemize graphics ++ map itemize pads)

newtype TEdit = TEdit String
instance Itemizable TEdit where
  itemize (TEdit s)= Item "tedit" [PString s]

newtype TStamp = TStamp String
instance Itemizable TStamp where
  itemize (TStamp s)= Item "tstamp" [PString s]


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

data Pad = Pad
  Int       -- ^ Pin number
  PadType   -- ^ Through hole or SMD
  PadShape
  Position
  Size
  PadDrill
  Layer
  Net

data PadType = ThroughHole | SMD
instance Itemizable PadType where
  itemize ThroughHole = PString "thru_hole"
  itemize SMD = PString "smd"

data PadShape = Circle | Rect | Oval
instance Itemizable PadShape where
  itemize Circle = PString "circle"
  itemize Rect = PString "rect"
  itemize Oval = PString "oval"

newtype PadDrill = PadDrill Float
instance Itemizable PadDrill where
  itemize (PadDrill f) = Item "drill" [PFloat f]

instance Itemizable Pad where
  itemize (Pad number padType shape pos size drill layer net) =
    Item "Pad" [PInt number, itemize padType, itemize shape, itemize pos, itemize size, itemize drill, itemize layer, itemize net]

data Graphic = FpLine
  (V2 Float)  -- ^ Line start
  (V2 Float)  -- ^ Line end
  Layer       -- ^ Layer
  Float       -- ^ Line width

instance Itemizable Graphic where
  itemize (FpLine (V2 xs ys) (V2 xe ye) l w) = Item "fp_line" [Item "start" [PFloat xs, PFloat ys] , Item "end" [PFloat xe, PFloat ye], itemize l, PFloat w]
