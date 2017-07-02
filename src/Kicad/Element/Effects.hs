module Kicad.Element.Effects (
  Effects(..)
) where

import Kicad.SExpr

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
