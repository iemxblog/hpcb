module Hpcb.Data.Net (
  Net(..)
) where

import Hpcb.SExpr

data Net = Net Int String
instance Itemizable Net where
  itemize (Net n s) = Item "net" [PInt n, PString s]
