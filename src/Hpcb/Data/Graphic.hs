module Hpcb.Data.Graphic (
  Graphic(..)
) where

import Hpcb.SExpr
import Hpcb.Data.Action
import Hpcb.Data.Base
import Hpcb.Data.Layer
import Hpcb.Data.Effects

data Graphic =
  GrLine (V2 Float) (V2 Float) Float Layer Float  -- ^ start, end, angle, layer, width
  | GrCircle (V2 Float) (V2 Float) Layer Float    -- ^ center, end, layer, width
  | GrText String Position Layer Effects
  deriving Show

instance Itemizable Graphic where
  itemize (GrLine (V2 xs ys) (V2 xe ye) a l w) =
    Item "gr_line" [
      Item "start" [PFloat xs, PFloat ys],
      Item "end" [PFloat xe, PFloat ye],
      Item "angle" [PFloat a],
      itemize l,
      Item "width" [PFloat w]
      ]

  itemize (GrCircle (V2 xc yc) (V2 xe ye) l w) =
    Item "gr_circle" [
      Item "center" [PFloat xc, PFloat yc],
      Item "end" [PFloat xe, PFloat ye],
      itemize l,
      Item "width" [PFloat w]
    ]

  itemize (GrText s pos l e) =
    Item "gr_text" [
      PString $ show s,
      itemize pos,
      itemize l,
      itemize e
    ]

instance Transformable Graphic where
  transform m (GrLine s e a l w) = GrLine (applyMatrixV2 m s) (applyMatrixV2 m e) a l w
  transform m (GrCircle c e l w) = GrCircle (applyMatrixV2 m c) (applyMatrixV2 m e) l w
  transform m (GrText s pos l e) = GrText s (applyMatrix m pos) l e

instance Parameterized Graphic where
  layer l (GrLine s e a _ w) = GrLine s e a l w
  layer l (GrCircle c e _ w) = GrCircle c e l w
  layer l (GrText s pos _ e) = GrText s pos l e

  layers _ GrLine{} = error "A line cannot be on multiple layers"
  layers _ GrCircle{} = error "A circle cannot be on multiple layers"
  layers _ GrText{} = error "A text cannot be on multiple layers"

  width w (GrLine s e a l _) = GrLine s e a l w
  width w (GrCircle c e l _) = GrCircle c e l w
  width _ (GrText s pos l e) = GrText s pos l e

  effects _ l@GrLine{} = l
  effects _ c@GrCircle{} = c
  effects f (GrText s pos lay e) = GrText s pos lay (f e)
