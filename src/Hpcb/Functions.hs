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
  line,
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

-- | Function that builds a new footprint.
footprint ::   String    -- ^ Reference
            -> String    -- ^ Name
            -> FpContent
            -> Circuit
footprint ref n fpc = Circuit [Footprint ref n defaultLayer dummyTEdit dummyTStamp origin fpc] [] []

-- | Creates a line in a footprint.
fpLine ::   V2 Float    -- ^ Start
            -> V2 Float -- ^ End
            -> FpContent
fpLine s e = FpContent [FpLine s e defaultLayer defaultWidth]

-- | Creates a polygon in a footprint.
fpPolygon ::  [V2 Float]    -- ^ List of points of the polygon
              -> FpContent
fpPolygon xs = FpContent $ map (\(s, e) -> FpLine s e defaultLayer defaultWidth) l
  where l = zip xs (tail (cycle xs))

-- | Creates a rectangle in a footprint.
-- It is centered on the origin of the footprint.
-- It may be translated after with 'translate', or rotated with 'rotate'.
fpRectangle ::  Float     -- ^ Width
                -> Float  -- ^ Height
                -> FpContent
fpRectangle w h = fpPolygon [
  V2 (-w/2) (-h/2),
  V2 (w/2) (-h/2),
  V2 (w/2) (h/2),
  V2 (-w/2) (h/2)
  ]

-- | Creates a circle in a footprint.
-- It is centered on the origin of the footprint.
-- It may be translated after with 'translate'.
fpCircle :: Float         -- ^ Diameter
            -> FpContent
fpCircle d = FpContent [FpCircle (V2 0 0) (V2 (d/2) 0) defaultLayer defaultWidth]

-- | Creates a text in a footprint.
-- It is centered on the origin of the footprint.
-- It may be translated after with 'translate', or rotated with 'rotate'.
fpText :: String                -- ^ Name
          -> String             -- ^ Content
          -> Effects            -- ^ Effects (Font, justification, etc.)
          -> FpContent
fpText n c e = FpContent [FpText n c origin defaultLayer e]

-- | Creates a pad.
-- The pad is centered on the origin of the footprint.
-- It may be translated after with 'translate', or rotated with 'rotate'.
pad ::  Int           -- ^ Pin number
        -> PadType
        -> PadShape
        -> V2 Float   -- ^ Size
        -> Net
        -> FpContent
pad n t sh si pnet = FpContent [Pad n [] t sh origin si padDefaultLayers pnet]

-- | Generates a generic net name, given the reference of a component and a pin number.
-- For example :
--
-- @
-- newNet "R1" 1
-- @
-- returns "Net-(R1-Pad)"
newNet ::  String    -- ^ Reference of component
        -> Int    -- ^ Pin number
        -> Net
newNet ref pin = Net $ "Net-(" ++ ref ++ "-Pad" ++ show pin ++")"

-- ############ Graphic Functions

-- | Creates a line (which does not belong to a footprint)
line :: V2 Float
        -> V2 Float
        -> Circuit
line a b = Circuit [] [GrLine a b angle defaultLayer defaultWidth] []
  where angle = 90 -- I don't know what it is useful for... not found in the doc

-- | Creates a polygon
polygon :: [V2 Float]    -- ^ List of points of the path
          -> Circuit
polygon xs = mconcat . map (uncurry line) $ l
  where
    l = zip xs (tail (cycle xs))

-- | Creates a rectangle, centered on the origin.
-- It is centered on the origin of the footprint.
-- It may be translated after with 'translate', or rotated with 'rotate'.
rectangle ::  Float     -- ^ Width
              -> Float  -- ^ Height
              -> Circuit
rectangle w h = polygon [
  V2 (-w/2) (-h/2),
  V2 (w/2) (-h/2),
  V2 (w/2) (h/2),
  V2 (-w/2) (h/2)
  ]

-- | Creates a text.
-- It is centered on the origin of the footprint.
-- It may be translated after with 'translate', or rotated with 'rotate'.
text :: String
        -> Circuit
text s = Circuit [] [GrText s origin FSilkS defaultEffects] []

-- | Prints the output of a given circuit, in Kicad file format.
runCircuit :: Circuit -> IO ()
runCircuit = putStr . prettyPrint . kicadPCB . numberNets
