module Kicad.SExpr (
    Item(..)
    , Itemizable(..)
) where
import Data.List
import Numeric

-- | Item type used to represent S-Expression of Kicad format.
-- Example :
--
-- (via (at 152 98))
--
-- Would be created like this with our type :
--
-- @
-- Item "via" [ParamItem $ Item "at" [ParamInt 152, ParamInt 98]]
-- @
data Item =
    Item String [Item]
    | PString String
    | PFloat Float
    | PInt Int

instance Show Item where
    show (Item s xs) = "(" ++ s ++ " " ++ (unwords . map show) xs ++ ")"
    show (PString s) = show s
    show (PFloat f) = showFFloat Nothing f ""
    show (PInt i) = show i

class Itemizable a where
    itemize :: a -> Item
