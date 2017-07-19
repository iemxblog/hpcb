module Hpcb.Data.Footprint(
  Footprint(..),
  FpContent(..),
  _fpContent,
  _fpElements
) where

import Hpcb.SExpr
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.FpElement
import Hpcb.Data.Action
import Control.Lens hiding (transform)
import Data.Matrix

data Footprint = Footprint {
  getFpName :: String,
  getFplayer :: Layer,
  getFpTEdit :: TEdit,
  getFpTStamp :: TStamp,
  getFpPosition :: Position,
  getFpContent :: FpContent
  } deriving Show

instance Itemizable Footprint where
  itemize (Footprint n l te ts pos (FpContent fpContent)) =
    Item "module" ([PString n, itemize l, itemize te, itemize ts, itemize pos] ++ map itemize fpContent)

instance Transformable Footprint where
  transform m (Footprint s l te ts pos fpc) = Footprint s l te ts (applyMatrix m pos) (transform newM fpc)
    where
      newM = setElem o (3, 4) $ identity 4  -- we keep only the orientation of the footprint
      o = getElem 3 4 m                     -- we get the orientation of the footprint

instance Parameterized Footprint where
  layer l (Footprint s _ te ts pos fpc) = Footprint s l te ts pos fpc
  layers _ Footprint{} = error "A footprint can have multiple layers"
  width w (Footprint s l te ts pos fpc) = Footprint s l te ts pos (width w fpc)

newtype FpContent = FpContent { getFpElements :: [FpElement] } deriving Show

instance Transformable FpContent where
  transform f (FpContent fpc) = FpContent $ map (transform f) fpc

instance Monoid FpContent where
  mempty = FpContent []
  FpContent xs `mappend` FpContent ys = FpContent (xs ++ ys)

instance Parameterized FpContent where
  layer l (FpContent fpc) = FpContent (map (layer l) fpc)
  layers ls (FpContent fpc) = FpContent (map (layers ls) fpc)
  width w (FpContent fpc) = FpContent (map (width w) fpc)

-- Lenses
_fpContent :: Lens' Footprint FpContent
_fpContent = lens getFpContent (\footprint fpcontent -> footprint {getFpContent = fpcontent})

_fpElements :: Lens' FpContent [FpElement]
_fpElements = lens getFpElements (\fpcontent fpelements -> fpcontent {getFpElements = fpelements})
