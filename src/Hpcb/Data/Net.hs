module Hpcb.Data.Net (
  Net(..)
) where

import Hpcb.SExpr

-- | Net datatype.
-- The 'NumberedNet' constructor is used only when transforming
-- the circuit to Kicad file format, because Kicad needs the nets
-- to be numbered. Otherwise in the program, 'Net' is the only used
-- constructor.
data Net =  Net {netName :: String}
            | NumberedNet {netNumber ::Int, netName :: String}
            deriving (Eq, Ord, Show)

instance Itemizable Net where
  itemize Net{} = error "Net must be numbered before itemizing"
  itemize (NumberedNet n s) = Item "net" [PInt n, PString ("\"" ++ s ++ "\"")]
