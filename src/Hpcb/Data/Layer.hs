{-# LANGUAGE FlexibleInstances #-}
module Hpcb.Data.Layer(
  Layer(..),
  topSideLayers,
  bottomSideLayers,
  copperLayers,
  maskLayers,
  defaultLayer,
  padDefaultLayers
) where

import Hpcb.SExpr

data Layer =
  FCu
  | BCu
  | FAdhes
  | BAdhes
  | FPaste
  | BPaste
  | FSilkS
  | BSilkS
  | FMask
  | BMask
  | DwgsUser
  | CmtsUser
  | Eco1User
  | Eco2User
  | EdgeCuts
  | Margin
  | FCrtYd
  | BCrtYd
  | FFab
  | BFab

instance Show Layer where
  show FCu = "F.Cu"
  show BCu = "B.Cu"
  show FAdhes = "F.Adhes"
  show BAdhes = "B.Adhes"
  show FPaste = "F.Paste"
  show BPaste = "B.Paste"
  show FSilkS = "F.SilkS"
  show BSilkS = "B.SilkS"
  show FMask = "F.Mask"
  show BMask = "B.Mask"
  show DwgsUser = "Dwgs.User"
  show CmtsUser = "Cmts.User"
  show Eco1User = "Eco1.User"
  show Eco2User = "Eco2.User"
  show EdgeCuts = "Edge.Cuts"
  show Margin = "Margin"
  show FCrtYd = "F.CrtYd"
  show BCrtYd = "B.CrtYd"
  show FFab = "F.Fab"
  show BFab = "B.Fab"

-- | Layer management instance. We put an s wheen there a several layers.
-- This instance is why we need FlexibleInstances.
instance Itemizable [Layer] where
  itemize [] = error "Empty list of layers"
  itemize [l] = Item "layer" [itemize l]
  itemize ls = Item "layers" $ map (PString . show) ls


instance Itemizable Layer where
  itemize FCu = Item "layer" [PString "F.Cu"]
  itemize BCu = Item "layer" [PString "B.Cu"]
  itemize FAdhes = Item "layer" [PString "F.Adhes"]
  itemize BAdhes = Item "layer" [PString "B.Adhes"]
  itemize FPaste = Item "layer" [PString "F.Paste"]
  itemize BPaste = Item "layer" [PString "B.Paste"]
  itemize FSilkS = Item "layer" [PString "F.SilkS"]
  itemize BSilkS = Item "layer" [PString "B.Silks"]
  itemize FMask = Item "layer" [PString "F.Mask"]
  itemize BMask = Item "layer" [PString "B.Mask"]
  itemize DwgsUser = Item "layer" [PString "Dwgs.User"]
  itemize CmtsUser = Item "layer" [PString "Cmts.User"]
  itemize Eco1User = Item "layer" [PString "Eco1.User"]
  itemize Eco2User = Item "layer" [PString "Eco2.User"]
  itemize EdgeCuts = Item "layer" [PString "Edge.Cuts"]
  itemize Margin = Item "layer" [PString "Margin"]
  itemize FCrtYd = Item "layer" [PString "F.CrtYd"]
  itemize BCrtYd = Item "layer" [PString "B.CrtYd"]
  itemize FFab = Item "layer" [PString "F.Fab"]
  itemize BFab = Item "layer" [PString "B.Fab"]

topSideLayers :: [Layer]
topSideLayers =  [FCu, FAdhes, FPaste, FSilkS, FMask, FCrtYd, FFab]

bottomSideLayers :: [Layer]
bottomSideLayers = [BCu, BAdhes, BPaste, BSilkS, BMask, BCrtYd, BFab]

copperLayers :: [Layer]
copperLayers = [FCu, BCu]

maskLayers :: [Layer]
maskLayers = [FMask, BMask]

defaultLayer :: Layer
defaultLayer = FCu

padDefaultLayers :: [Layer]
padDefaultLayers = copperLayers ++ maskLayers ++ [FSilkS]
