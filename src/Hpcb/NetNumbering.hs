module Hpcb.NetNumbering (
  listOfNets,
  netsMap,
  numberNets
) where

import Hpcb.Data.Net
import Hpcb.Data.FpElement
import Hpcb.Data.Circuit
import Hpcb.Data.Footprint
import qualified Data.Map as Map
import Control.Lens


-- | Returns the list of nets in a circuit
listOfNets :: Circuit -> [Net]
listOfNets = toListOf (_footprints . traverse . _fpContent . _fpElements . traverse . _pad . _net)

-- | Returns a map of net names to their number
netsMap :: Circuit -> Map.Map Net Int
netsMap c = Map.fromList $ zip (listOfNets c) [1..]

-- | Assigns its number to a Net
numberNet :: Map.Map Net Int -> Net -> Net
numberNet m n@(Net nn) = case Map.lookup n m of
                          Nothing -> error $ "Error while looking up the net number of net " ++ show nn
                          Just n -> NumberedNet n nn
numberNet m (NumberedNet n nn) = NumberedNet n nn

-- | Assigns its nulber to every net of the 'Circuit'
numberNets :: Circuit -> Circuit
numberNets c = over (_footprints . traverse . _fpContent . _fpElements . traverse . _pad . _net) (numberNet (netsMap c)) c
  where nm = netsMap

{-
numberNetFpElement :: Map.Map String Int -> FpElement -> FpElement
numberNetFpElement

numberNetsFootprint ::  Data.Map String Int
                        -> Footprint
                        -> Footprint
numberNetsFootprint m (Footprint n l te ts pos (FpContent fpc))


numberNetsSegment ::    Data.Map String Int
                        -> Segment
                        -> Segment
numberNetsSegment =


numberNets :: Circuit -> Circuit
-}
