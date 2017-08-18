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

-- | Datatype that represents Kicad file format's effects.
data Effects = Effects Font Display deriving Show

-- | Font type : size, thickness and 'Style'.
data Font = Font (Float, Float) Float Style deriving Show
-- | Style of a 'Font' : Normal or italic.
data Style = Normal | Italic deriving Show

data Display = Display Justification Bool deriving Show

-- | Font justification.
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

-- | Effects that are used when generating a text.
-- They can be modified later by using functions of the 'Parameterized' typeclass.
defaultEffects :: Effects
defaultEffects = Effects (Font (1,1) 0.15 Normal) (Display CenterJustify False)

-- | Modifies the size of a font.
-- This function is used by the 'Parameterized' typeclass.
fontSizeE :: (Float, Float) -- ^ New size
            -> Effects
            -> Effects
fontSizeE si (Effects (Font _ th st) d) = Effects (Font si th st) d

-- | Modifies the thickness of a font.
-- This function is used by the 'Parameterized' typeclass.
fontThicknessE ::  Float      -- ^ New thickness
                  -> Effects
                  -> Effects
fontThicknessE th (Effects (Font si _ st) d) = Effects (Font si th st) d

-- | Modifies the style of a font.
-- This function is used by the 'Parameterized' typeclass.
fontStyleE ::  Style        -- ^ New style
              -> Effects
              -> Effects
fontStyleE st (Effects (Font si th _) d) = Effects (Font si th st) d

-- | Modifies the justification of a text.
-- This function is used by the 'Parameterized' typeclass.
justifyE ::  Justification
                  -> Effects
                  -> Effects
justifyE j (Effects f (Display _ b)) = Effects f (Display j b)

-- | Applies mirroring to a text.
-- This function is used by the 'Parameterized' typeclass.
mirrorE :: Effects -> Effects
mirrorE (Effects f (Display j _)) = Effects f (Display j True)
