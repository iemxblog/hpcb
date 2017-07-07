module Hpcb.Data.Net (
  Net(..)
) where

import Hpcb.SExpr

data Net =  Net String
            | NumberedNet Int String

instance Itemizable Net where
  itemize Net{} = error "Net must be numbered before itemizing"
  itemize (NumberedNet n s) = Item "net" [PInt n, PString ("\"" ++ s ++ "\"")]
