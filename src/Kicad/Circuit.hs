module Kicad.Circuit (
  Circuit,
  runCircuit,
  translate,
  rotate,
  layer,
  footprint,
  fpLine,
  fpCircle,
  fpText,
  pad
) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.Footprint
import Kicad.Element.FpElement
import Kicad.Element.Effects
import Kicad.Element.Net
import Control.Monad.Reader

type Circuit a = Reader Environment a

data Environment = Environment {
  tr :: Transformation,
  lay :: Layer
}

runCircuit :: Circuit a -> a
runCircuit = flip runReader Environment {tr = id, lay = FCu}

getTransformation :: Circuit Transformation
getTransformation = asks tr

-- | This function is used in 'footprint' to reset the current transformation,
-- because footprint element coordinates are relative to the footprint center
withTransformation :: Transformation -> Circuit a -> Circuit a
withTransformation f = local (\e -> e{tr=f})

getPosition :: Circuit Position
getPosition = liftM ($ origin) getTransformation

getLayer :: Circuit Layer
getLayer = asks lay

transform :: Transformation -> Circuit a -> Circuit a
transform f = local (\e -> let trEnv = tr e in e {tr = f . trEnv})

translate :: V2 Float -> Circuit a -> Circuit a
translate v = transform $ translation v

rotate :: Float -> Circuit a -> Circuit a
rotate angle = transform $ rotation angle

layer :: Layer -> Circuit a -> Circuit a
layer l = local (\e -> e {lay = l})


footprint ::   String    -- ^ Name
            -> Circuit [FpElement]
            -> Circuit Footprint
footprint n f = do
  la <- getLayer
  pos <- getPosition
  es <- withTransformation id f -- we reset the transformation when calling f, because footprint element coordinates are relative to the footprint
  return $ Footprint n la dummyTEdit dummyTStamp pos es

fpLine ::   V2 Float    -- ^ Start
            -> V2 Float -- ^ End
            -> Float    -- ^ Width
            -> Circuit [FpElement]
fpLine s e w = getLayer >>= \l -> return [FpLine s e l w]

fpCircle :: V2 Float    -- ^ Center
            -> V2 Float -- ^ End
            -> Float    -- ^ Width
            -> Circuit [FpElement]
fpCircle c e w = getLayer >>= \l -> return [FpCircle c e l w]

fpText :: String                -- ^ Name
          -> String             -- ^ Content
          -> Effects            -- ^ Effects (Font, justification, etc.)
          -> Circuit [FpElement]
fpText n c e = do
  l <- getLayer
  pos <- getPosition
  return [FpText n c pos l e]

pad ::  Int
        -> PadType
        -> PadShape
        -> Size
        -> PadDrill
        -> Net
        -> Circuit [FpElement]
pad n t sh si d pnet = do
  l <- getLayer
  pos <- getPosition
  return [Pad n t sh pos si d [l] pnet] -- Only single layer is supported now, multiple layers will have to be implemented soon
