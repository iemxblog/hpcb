module Kicad.Data.Net (
  Net(..)
) where

import Kicad.SExpr

data Net = Net Int String
instance Itemizable Net where
  itemize (Net n s) = Item "net" [PInt n, PString s]
