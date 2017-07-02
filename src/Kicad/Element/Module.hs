module Kicad.Element.Module(
  Module(..)
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.FpText
import Kicad.Element.FpLine
import Kicad.Element.Pad

data Module = Module
  String    -- ^ Module Name
  Layer     -- ^ Module layer
  TEdit     -- ^ Last edition time stamp
  TStamp    -- ^ Time stamp from the schematic
  Position  -- ^ Module position
  [FpText]
  [FpLine]
  [Pad]

instance Itemizable Module where
  itemize (Module n l te ts pos texts graphics pads) =
    Item "Module" ([PString n, itemize l, itemize te, itemize ts, itemize pos] ++ map itemize texts ++ map itemize graphics ++ map itemize pads)
