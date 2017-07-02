module Kicad.Element.Footprint(
  Footprint(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.FpGraphic
import Kicad.Element.Pad

data Footprint = Footprint String Layer TEdit TStamp Position [FpGraphic] [Pad] -- ^ Name, layer, last edition time stamp, time stamp from the schematic, module position, ...

instance Itemizable Footprint where
  itemize (Footprint n l te ts pos fpgraphics pads) =
    Item "Module" ([PString n, itemize l, itemize te, itemize ts, itemize pos] ++ map itemize fpgraphics ++ map itemize pads)
