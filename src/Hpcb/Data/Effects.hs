module Hpcb.Data.Effects (
  Effects(..),
  Style(..),
  Justification(..),
  defaultEffects,
  fontSizeE,
  fontThicknessE,
  fontStyleE,
  justifyE,
  mirrorE
) where

import Hpcb.SExpr

-- | Effects are not implemented yet
-- There is just a single constructor to provide one effect
data Effects = Effects Font Display deriving Show
data Font = Font (Float, Float) Float Style deriving Show
data Style = Normal | Italic deriving Show

data Display = Display Justification Bool deriving Show
data Justification = LeftJustify | CenterJustify | RightJustify deriving Show

instance Itemizable Effects where
  itemize (Effects f d)  =
      Item "effects" [
        itemize f,
        itemize d
      ]

instance Itemizable Font where
  itemize (Font (sx, sy) th s) =
    Item "font" [
      Item "size" [PFloat sx, PFloat sy],
      Item "thickness" [PFloat th],
      itemize s
    ]

instance Itemizable Style where
  itemize Normal = PString ""
  itemize Italic = PString "italic"

instance Itemizable Display where
  itemize (Display CenterJustify False) = PString ""
  itemize (Display j b) = Item "justify" [itemize j, if b then PString "mirror" else PString ""]

instance Itemizable Justification where
  itemize LeftJustify = PString "left"
  itemize CenterJustify = PString "center"
  itemize RightJustify = PString "right"

defaultEffects :: Effects
defaultEffects = Effects (Font (1,1) 0.15 Normal) (Display CenterJustify False)

fontSizeE :: (Float, Float)
            -> Effects
            -> Effects
fontSizeE si (Effects (Font _ th st) d) = Effects (Font si th st) d

fontThicknessE ::  Float
                  -> Effects
                  -> Effects
fontThicknessE th (Effects (Font si _ st) d) = Effects (Font si th st) d

fontStyleE ::  Style
              -> Effects
              -> Effects
fontStyleE st (Effects (Font si th _) d) = Effects (Font si th st) d

justifyE ::  Justification
                  -> Effects
                  -> Effects
justifyE j (Effects f (Display _ b)) = Effects f (Display j b)

mirrorE :: Effects -> Effects
mirrorE (Effects f (Display j _)) = Effects f (Display j True)
