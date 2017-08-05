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
  rectangle,
  text,
  runCircuit
) where

import Hpcb.Data.Base
import Hpcb.Data.Circuit
import Hpcb.Data.Layer
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Graphic
import Hpcb.Data.Effects
import Hpcb.Data.Net
import Hpcb.Data.NetNumbering
import Hpcb.Data.KicadPCB
import Hpcb.SExpr

infixl 8 #
(#) :: a -> (a -> b) -> b
x # f = f x

footprint ::   String    -- ^ Reference
            -> String    -- ^ Name
            -> FpContent
            -> Circuit
footprint ref n fpc = Circuit [Footprint ref n defaultLayer dummyTEdit dummyTStamp origin fpc] [] []

fpLine ::   V2 Float    -- ^ Start
            -> V2 Float -- ^ End
            -> FpContent
fpLine s e = FpContent [FpLine s e defaultLayer defaultWidth]

fpPolygon ::  [V2 Float]    -- ^ List of points of the path
              -> FpContent
fpPolygon xs = FpContent $ map (\(s, e) -> FpLine s e defaultLayer defaultWidth) l
  where l = zip xs (tail (cycle xs))

fpRectangle ::  Float     -- ^ Width
                -> Float  -- ^ Height
                -> FpContent
fpRectangle w h = fpPolygon [
  V2 (-w/2) (-h/2),
  V2 (w/2) (-h/2),
  V2 (w/2) (h/2),
  V2 (-w/2) (h/2)
  ]

fpCircle :: Float         -- ^ Diameter
            -> FpContent
fpCircle d = FpContent [FpCircle (V2 0 0) (V2 (d/2) 0) defaultLayer defaultWidth]

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
pad n t sh si pnet = FpContent [Pad n [] t sh origin si padDefaultLayers pnet]

newNet ::  String    -- ^ Reference of component
        -> Int    -- ^ Pin number
        -> Net
newNet ref pin = Net $ "Net-(" ++ ref ++ "-Pad" ++ show pin ++")"

-- ############ Graphic Functions

polygon :: [V2 Float]    -- ^ List of points of the path
          -> Circuit
polygon xs = Circuit [] (map (\(s, e) -> GrLine s e angle defaultLayer defaultWidth) l) []
  where
    l = zip xs (tail (cycle xs))
    angle = 90 -- I don't know what it is useful for... not found in the doc

rectangle ::  Float     -- ^ Width
              -> Float  -- ^ Height
              -> Circuit
rectangle w h = polygon [
  V2 (-w/2) (-h/2),
  V2 (w/2) (-h/2),
  V2 (w/2) (h/2),
  V2 (-w/2) (h/2)
  ]

text :: String
        -> Circuit
text s = Circuit [] [(GrText s origin FSilkS defaultEffects)] []

runCircuit :: Circuit -> IO ()
runCircuit = putStr . prettyPrint . kicadPCB . numberNets
