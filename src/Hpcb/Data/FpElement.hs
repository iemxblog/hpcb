module Hpcb.Data.FpElement (
  FpElement(..),
  PadType(..),
  PadShape(..),
  _pad,
  _net
) where

import Hpcb.SExpr
import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Effects
import Hpcb.Data.Net
import Control.Lens

data FpElement =
  FpLine {
    fpLineStart :: V2 Float,
    fpLineEnd :: V2 Float,
    fpLineLayer :: Layer,
    fpLineWidth :: Float }
  | FpCircle {
    fpCircleCenter :: V2 Float,
    fpCircleEnd :: V2 Float,
    fpCircleLayer :: Layer,
    fpCircleWidth :: Float }
  | FpText {
    fpTextName :: String,
    fpTextContent :: String,
    fpTextPos :: Position,
    fpTextLayer :: Layer,
    fpTextEffects :: Effects }
  | Pad {
    padNumber :: Int,
    padNames :: [String],
    padType :: PadType,
    padShape :: PadShape,
    padPos :: Position,
    padSize :: V2 Float,
    padLayers :: [Layer],
    padNet :: Net }
  deriving Show

instance Itemizable FpElement where
  itemize (FpLine (V2 xs ys) (V2 xe ye) l w) =
    Item "fp_line" [
      Item "start" [PFloat xs, PFloat ys] ,
      Item "end" [PFloat xe, PFloat ye],
      itemize l,
      Item "width" [PFloat w]
    ]

  itemize (FpCircle (V2 xc yc) (V2 xe ye) l w) =
    Item "fp_circle" [
      Item "center" [PFloat xc, PFloat yc],
      Item "end" [PFloat xe, PFloat ye],
      itemize l,
      Item "width" [PFloat w]
    ]

  itemize (FpText name text pos lay effects) =
    Item "fp_text" [
      PString name,
      PString text,
      itemize pos,
      itemize lay,
      itemize effects
    ]

  itemize (Pad number _ padType shape pos (V2 sizeX sizeY) ls net) =
    Item "pad" ([
      PInt number,
      itemize padType,
      itemize shape,
      itemize pos,
      Item "size" [PFloat sizeX, PFloat sizeY]]
      ++ d ++
      [itemize ls,
      itemize net
    ])
    where d = case padType of
                (ThroughHole dd) -> [Item "drill" [PFloat dd]]
                SMD -> []

instance Transformable FpElement where
  transform m (FpLine s e l w) = FpLine (applyMatrixV2 m s) (applyMatrixV2 m e) l w
  transform m (FpCircle c e l w) = FpCircle (applyMatrixV2 m c) (applyMatrixV2 m e) l w
  transform m (FpText name text pos lay effects) = FpText name text (applyMatrix m pos) lay effects
  transform m (Pad number padNames padType shape pos size ls net) =
    Pad number padNames padType shape (applyMatrix m pos) size ls net

instance Parameterized FpElement where
  layer l (FpLine s e _ w) = FpLine s e l w
  layer l (FpCircle c e _ w) = FpCircle c e l w
  layer l (FpText n t pos _ e) = FpText n t pos l e
  layer l (Pad number padNames padType shape pos size _ net) =
    Pad number padNames padType shape pos size [l] net

  layers _ FpLine{} = error "A fp_line cannot have multiple layers"
  layers _ FpCircle{} = error "A fp_circle cannot have multiple layers"
  layers _ FpText{} = error "A fp_text cannot have multiple layers"
  layers ls (Pad number padNames padType shape pos size _ net) =
    Pad number padNames padType shape pos size ls net

  width w (FpLine s e l _) = FpLine s e l w
  width w (FpCircle c e l _) = FpCircle c e l w
  width _ (FpText n t pos l e) = FpText n t pos l e
  width _ (Pad number padNames padType shape pos size l net) =
    Pad number padNames padType shape pos size l net


data PadType = ThroughHole {getDrill :: Float} | SMD deriving Show
instance Itemizable PadType where
  itemize (ThroughHole _) = PString "thru_hole"
  itemize SMD = PString "smd"

data PadShape = Circle | Rect | Oval deriving Show
instance Itemizable PadShape where
  itemize Circle = PString "circle"
  itemize Rect = PString "rect"
  itemize Oval = PString "oval"

-- Lenses

_pad :: Prism' FpElement FpElement
_pad = prism' setter getter
  where
    setter (Pad pn pnames pt sh pos si ls n) = Pad pn pnames pt sh pos si ls n
    setter fpe = fpe
    getter pad@Pad{} = Just pad
    getter _ = Nothing

_net :: Lens' FpElement Net
_net = lens getter setter
  where
    getter (Pad _ _ _ _ _ _  _ n) = n
    setter (Pad pn pnames pt sh pos si ls _) n = Pad pn pnames pt sh pos si ls n
