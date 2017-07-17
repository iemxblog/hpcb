module Hpcb.Functions (
  (#),
  footprint,
  fpLine,
  fpPath,
  fpRectangle,
  fpCircle,
  fpText,
  pad,
  newNet
) where

import Hpcb.SExpr
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Effects
import Hpcb.Data.Net
import Data.Monoid

infixl 8 #
x # f = f x

footprint ::   String    -- ^ Name
            -> FpContent
            -> Footprint
footprint n fpc = Footprint n defaultLayer dummyTEdit dummyTStamp origin fpc

fpLine ::   V2 Float    -- ^ Start
            -> V2 Float -- ^ End
            -> Float    -- ^ Width
            -> FpContent
fpLine s e w = FpContent [FpLine s e defaultLayer w]

fpPath :: [V2 Float]    -- ^ List of points of the path
          -> Bool       -- ^ True : Path forms a cycle (return to first point at the end)
          -> Float      -- ^ Width of the path
          -> FpContent
fpPath xs cy w = FpContent $ map (\(s, e) -> FpLine s e defaultLayer w) l
  where l = case cy of
              True -> zip xs (tail (cycle xs))
              False -> zip xs (tail xs)


fpRectangle ::  Float     -- ^ Width
                -> Float  -- ^ Height
                -> Float  -- ^ Line width
                -> FpContent
fpRectangle w h lw =
  fpLine (V2 (-w/2) (h/2)) (V2 (-w/2) (-h/2)) lw
  <> fpLine (V2 (w/2) (h/2)) (V2 (-w/2) (h/2)) lw
  <> fpLine (V2 (w/2) (-h/2)) (V2 (w/2) (h/2)) lw
  <> fpLine (V2 (-w/2) (-h/2)) (V2 (w/2) (-h/2)) lw

fpCircle :: V2 Float    -- ^ Center
            -> V2 Float -- ^ End
            -> Float    -- ^ Width
            -> FpContent
fpCircle c e w = FpContent [FpCircle c e defaultLayer w]

fpText :: String                -- ^ Name
          -> String             -- ^ Content
          -> Effects            -- ^ Effects (Font, justification, etc.)
          -> FpContent
fpText n c e = FpContent [FpText n c origin defaultLayer e]

pad ::  Int           -- ^ Pin number
        -> PadType
        -> PadShape
        -> V2 Float   -- ^ Size
        -> Net
        -> FpContent
pad n t sh si pnet = FpContent [Pad n t sh origin si padDefaultLayers pnet]

newNet ::  String    -- ^ Reference of component
        -> Int    -- ^ Pin number
        -> Net
newNet ref pin = Net $ "Net-(" ++ ref ++ "-Pad" ++ show pin ++")"
