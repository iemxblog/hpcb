module Kicad (
    Item(..)
    , Parameter(..)
) where
import Data.List

-- | 
data Item = 
    Item String [Parameter]

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
