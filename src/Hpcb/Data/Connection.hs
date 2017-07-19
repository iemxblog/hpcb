module Hpcb.Data.Connection (
  connect
) where

import Hpcb.Data.Circuit
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Net
import Hpcb.Data.Segment
import Control.Lens
{-
getPinNet ::  String
              -> Int
              -> String
getPinNet =
  where
-}

-- | Replaces occurences of Net name 2 by Net name 1
-- Beware, connect s1 s2 is different from connect s2 s1
connect ::  String      -- ^ Net name 1
            -> String   -- ^ Net name 2
            -> Circuit
            -> Circuit
connect nn1 nn2 c = c2
  where
    f (Net nn) | nn == nn2 = Net nn1
    f (Net nn) = Net nn
    f (NumberedNet _ _) = error $ "Cannot connect a numbered net (nn1 = " ++ nn1 ++ " nn2 = " ++ nn2 ++")"
    c1 = over (_footprints . traverse . _fpContent . _fpElements . traverse . _pad . _net) f c
    c2 = over (_segments . traverse . _segNet) f c1
