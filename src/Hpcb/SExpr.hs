module Hpcb.SExpr (
    Item(..)
    , Itemizable(..)
    , prettyPrint
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
-- Item "via" [Item "at" [PInt 152, PInt 98]]
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


indent :: Bool -> Int -> String -> String
indent True n s = replicate (4*n) ' ' ++ s ++ "\n"
indent False n s = s

noIndent :: [String]
noIndent = ["at", "drill", "size", "net", "layer", "start", "end", "effects", "layer", "layers", "width", "tedit", "tstamp"]

prettyPrint' :: Bool -> Int -> Item -> String
prettyPrint' b n (PString s) = indent b n s
prettyPrint' b n (PFloat f) = indent b n $ showFFloat Nothing f ""
prettyPrint' b n (PInt i) = indent b n $ show i
prettyPrint' b n (Item s xs) | s `elem` noIndent = indent b n $ "(" ++ s ++ " " ++ unwords (map (prettyPrint' False n) xs) ++ ")"
  | otherwise = indent b n ("(" ++ s ++ " ") ++ concatMap (prettyPrint' b (n+1)) xs ++ indent b n ")"

prettyPrint :: Item -> String
prettyPrint = prettyPrint' True 0
