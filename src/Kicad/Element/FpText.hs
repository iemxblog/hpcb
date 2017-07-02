module Kicad.Element.FpText(
  FpText(..),
  Effects(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.Effects

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
