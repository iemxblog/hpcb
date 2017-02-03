module Kicad (
    Item(..)
    , Parameter(..)
) where
import Data.List

-- | Item type used to represent S-Expression of Kicad format.
-- Example : 
--
-- (via (at 152 98))
--
-- Would be create like this with our type : 
--
-- @
-- Item "via" [ParamItem $ Item "at" [ParamInt 152, ParamInt 98]]
-- @
data Item = 
    Item String [Parameter]

-- | Parameters in a S-Expression. See 'Item'.
data Parameter = 
    ParamString String
    | ParamFloat Float
    | ParamInt Int
    | ParamItem Item

instance Show Item where
    show (Item s xs) = "(" ++ s ++ " " ++ (concat . intersperse " " . map show) xs ++ ")"

instance Show Parameter where
    show (ParamString s) = show s
    show (ParamFloat f) = show f
    show (ParamInt i) = show i
    show (ParamItem i) = show i

class Itemizable a where
    itemize :: a -> Item
