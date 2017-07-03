module Kicad.Circuit (

) where

import Kicad.SExpr
import Kicad.Element.Base
import Kicad.Element.Layer
import Kicad.Element.Footprint
import Kicad.Element.FpElement
import Control.Monad.Reader

type Circuit a = Reader Environment a

data Environment = Environment {
  tr :: Transformation,
  lay :: Layer
}

getTransformation :: Circuit Transformation
getTransformation = asks tr

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
  es <- f
  return $ Footprint n la dummyTEdit dummyTStamp pos es
