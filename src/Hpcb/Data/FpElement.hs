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
  FpLine (V2 Float) (V2 Float) Layer Float -- ^ line start, line end, layer, line width
  | FpCircle (V2 Float) (V2 Float) Layer Float -- ^ center, end, layer, width
  | FpText String String Position Layer Effects -- ^ name, content, position, layer, effects (font, justification , etc.)
  | Pad Int PadType PadShape Position (V2 Float) Float [Layer] Net  -- ^ Int : Pin number, type, shape, position, size, drill, layers, net
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

  itemize (FpText name text pos layer effects) =
    Item "fp_text" [
      PString name,
      PString text,
      itemize pos,
      itemize layer,
      itemize effects
    ]

  itemize (Pad number padType shape pos (V2 sizeX sizeY) drill layers net) =
    Item "pad" [
      PInt number,
      itemize padType,
      itemize shape,
      itemize pos,
      Item "size" [PFloat sizeX, PFloat sizeY],
      Item "drill" [PFloat drill],
      itemize layers,
      itemize net
    ]

instance Transformable FpElement where
  transform f (FpLine (V2 xs ys) (V2 xe ye) l w) =
    FpLine (V2 xs' ys') (V2 xe' ye') l w
    where At (V2 xs' ys') _ = f (At (V2 xs ys) Nothing)
          At (V2 xe' ye') _ = f (At (V2 xe ye) Nothing)
  transform f (FpCircle (V2 xc yc) (V2 xe ye) l w) =
    FpCircle (V2 xc' yc') (V2 xe' ye') l w
    where At (V2 xc' yc') _ = f (At (V2 xc yc) Nothing)
          At (V2 xe' ye') _ = f (At (V2 xe ye) Nothing)

  transform f (FpText name text pos layer effects) =
    FpText name text (f pos) layer effects

  transform f (Pad number padType shape pos size drill layers net) =
    Pad number padType shape (f pos) size drill layers net

instance ChangeableLayer FpElement where
  layer l (FpLine s e _ w) = FpLine s e l w
  layer l (FpCircle c e _ w) = FpCircle c e l w
  layer l (FpText n t pos _ e) = FpText n t pos l e
  layer l (Pad number padType shape pos size drill _ net) =
    Pad number padType shape pos size drill [l] net

  layers _ FpLine{} = error "A fp_line cannot have multiple layers"
  layers _ FpCircle{} = error "A fp_circle cannot have multiple layers"
  layers _ FpText{} = error "A fp_text cannot have multiple layers"
  layers ls (Pad number padType shape pos size drill _ net) =
    Pad number padType shape pos size drill ls net


data PadType = ThroughHole | SMD deriving Show
instance Itemizable PadType where
  itemize ThroughHole = PString "thru_hole"
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
    setter (Pad pn pt sh pos si dr ls n) = Pad pn pt sh pos si dr ls n
    setter fpe = fpe
    getter pad@Pad{} = Just pad
    getter _ = Nothing

_net :: Lens' FpElement Net
_net = lens getter setter
  where
    getter (Pad _ _ _ _ _ _ _ n) = n
    setter (Pad pn pt sh pos si dr ls _) n = Pad pn pt sh pos si dr ls n
