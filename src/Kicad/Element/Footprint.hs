module Kicad.Element.Footprint(
  Footprint(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.FpElement

data Footprint = Footprint String Layer TEdit TStamp Position [FpElement] -- ^ Name, layer, last edition time stamp, time stamp from the schematic, module position, ...

instance Itemizable Footprint where
  itemize (Footprint n l te ts pos fpElements) =
    Item "Module" ([PString n, itemize l, itemize te, itemize ts, itemize pos] ++ map itemize fpElements)
