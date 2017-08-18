{-|
Module      : Hpcb.SExpr
Description : Datatype for Kicad file format's S-Expressions
Copyright   : (c) Maxime ANDRE, 2016
License     : MIT
Maintainer  : iemxblog@gmail.com
Stability   : experimental
Portability : POSIX

This module contains the datatype used to represent S-Expressions of Kicad
format.
-}
module Hpcb.SExpr (
    Item(..)
    , Itemizable(..)
    , prettyPrint
) where

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

-- | Class of types which are transformable into an S-Expression.
class Itemizable a where
    itemize :: a -> Item

indent :: Bool -> Int -> String -> String
indent True n s = replicate (4*n) ' ' ++ s ++ "\n"
indent False _ s = s

noIndent :: [String]
noIndent = ["at", "drill", "size", "net", "layer", "start", "end", "effects",
  "layer", "layers", "width", "tedit", "tstamp", "fp_line", "page", "links",
  "no_connects", "area", "thickness", "drawings", "tracks", "zones", "modules",
  "nets", "version", "host"]

prettyPrint' :: Bool -> Int -> Item -> String
prettyPrint' b n (PString s) = indent b n s
prettyPrint' b n (PFloat f) = indent b n $ showFFloat Nothing f ""
prettyPrint' b n (PInt i) = indent b n $ show i
prettyPrint' b n (Item s xs) | s `elem` noIndent || not b = indent b n $ "(" ++ s ++ " " ++ unwords (map (prettyPrint' False n) xs) ++ ")"
  | otherwise = indent b n ("(" ++ s ++ " ") ++ concatMap (prettyPrint' b (n+1)) xs ++ indent b n ")"

-- | Pretty prints an 'Item' (S-Expression)
prettyPrint ::  Item        -- ^ S-Expression to be pretty-printed
                -> String
prettyPrint = prettyPrint' True 0
