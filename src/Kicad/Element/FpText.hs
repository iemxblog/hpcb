module Kicad.Element.FpText(
  FpText(..),
  Effects(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer

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
