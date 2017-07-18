module Hpcb.Functions (
  (#),
  footprint,
  fpLine,
  fpPolygon,
  fpRectangle,
  fpCircle,
  fpText,
  pad,
  newNet,
  polygon,
  rectangle
) where

import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Layer
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Graphic
import Hpcb.Data.Effects
import Hpcb.Data.Net

infixl 8 #
(#) :: a -> (a -> b) -> b
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

fpPolygon :: Float      -- ^ Width of the path
          -> [V2 Float]    -- ^ List of points of the path
          -> FpContent
fpPolygon w xs = FpContent $ map (\(s, e) -> FpLine s e defaultLayer w) l
  where l = zip xs (tail (cycle xs))

fpRectangle ::  Float     -- ^ Width
                -> Float  -- ^ Height
                -> Float  -- ^ Line width
                -> FpContent
fpRectangle w h lw = fpPolygon lw [
  V2 (-w/2) (-h/2),
  V2 (w/2) (-h/2),
  V2 (w/2) (h/2),
  V2 (-w/2) (h/2)
  ]

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

-- ############ Graphic Functions

polygon :: Float      -- ^ Width of the path
          -> [V2 Float]    -- ^ List of points of the path
          -> Circuit
polygon w xs = Circuit [] (map (\(s, e) -> GrLine s e angle defaultLayer w) l) []
  where
    l = zip xs (tail (cycle xs))
    angle = 90 -- I don't know what it is useful for... not found in the doc

rectangle ::  Float     -- ^ Width
              -> Float  -- ^ Height
              -> Float  -- ^ Line width
              -> Circuit
rectangle w h lw = polygon lw [
  V2 (-w/2) (-h/2),
  V2 (w/2) (-h/2),
  V2 (w/2) (h/2),
  V2 (-w/2) (h/2)
  ]
