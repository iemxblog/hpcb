module Kicad.Element.Footprint(
  Footprint(..),
  FpContent(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.FpElement
import Kicad.Element.Action

data Footprint = Footprint String Layer TEdit TStamp Position FpContent -- ^ Name, layer, last edition time stamp, time stamp from the schematic, module position, ...

instance Itemizable Footprint where
  itemize (Footprint n l te ts pos (FpContent fpContent)) =
    Item "Module" ([PString n, itemize l, itemize te, itemize ts, itemize pos] ++ map itemize fpContent)

instance Transformable Footprint where
  transform f (Footprint s l te ts pos fpc) = Footprint s l te ts (f pos) fpc

instance ChangeableLayer Footprint where
  layer l (Footprint s _ te ts pos fpc) = Footprint s l te ts pos fpc

newtype FpContent = FpContent [FpElement]

instance Transformable FpContent where
  transform f (FpContent fpc) = FpContent $ map (transform f) fpc

instance Monoid FpContent where
  mempty = FpContent []
  FpContent xs `mappend` FpContent ys = FpContent (xs ++ ys)

instance ChangeableLayer FpContent where
  layer l (FpContent fpc) = FpContent (map (layer l) fpc)
  layers ls (FpContent fpc) = FpContent (map (layers ls) fpc)
