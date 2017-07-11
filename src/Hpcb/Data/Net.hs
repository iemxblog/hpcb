module Hpcb.Data.Net (
  Net(..)
) where

import Hpcb.SExpr

data Net =  Net {netName :: String}
            | NumberedNet Int String
            deriving (Eq, Ord, Show)

instance Itemizable Net where
  itemize Net{} = error "Net must be numbered before itemizing"
  itemize (NumberedNet n s) = Item "net" [PInt n, PString ("\"" ++ s ++ "\"")]
