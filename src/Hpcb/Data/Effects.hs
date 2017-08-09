module Hpcb.Data.Effects (
  Effects(..),
  defaultEffects,
  fontSize,
  fontThickness,
  fontStyle,
  justification,
  mirror
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

fontSize :: (Float, Float)
            -> Effects
            -> Effects
fontSize si (Effects (Font _ th st) d) = Effects (Font si th st) d

fontThickness ::  Float
                  -> Effects
                  -> Effects
fontThickness th (Effects (Font si _ st) d) = Effects (Font si th st) d

fontStyle ::  Style
              -> Effects
              -> Effects
fontStyle st (Effects (Font si th _) d) = Effects (Font si th st) d

justification ::  Justification
                  -> Effects
                  -> Effects
justification j (Effects f (Display _ b)) = Effects f (Display j b)

mirror :: Effects -> Effects
mirror (Effects f (Display j _)) = Effects f (Display j True)
