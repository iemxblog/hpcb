module Kicad.Element.Footprint(
  Footprint(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.FpElement

data Footprint = Footprint String Layer TEdit TStamp Position FootprintContent -- ^ Name, layer, last edition time stamp, time stamp from the schematic, module position, ...

newtype FootprintContent = FootprintContent [FpElement]

instance Itemizable Footprint where
  itemize (Footprint n l te ts pos (FootprintContent fpContent)) =
    Item "Module" ([PString n, itemize l, itemize te, itemize ts, itemize pos] ++ map itemize fpContent)

instance Monoid FootprintContent where
  mempty = FootprintContent []
  FootprintContent xs `mappend` FootprintContent ys = FootprintContent (xs ++ ys)
