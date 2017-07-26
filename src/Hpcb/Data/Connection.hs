module Hpcb.Data.Connection (
  pin,
  net,
  connect
) where

import Hpcb.Data.Circuit
import Hpcb.Data.Footprint
import Hpcb.Data.FpElement
import Hpcb.Data.Net
import Hpcb.Data.Segment
import Control.Lens

-- | Returns the name of the net associated with the pin (identified by component reference and pin number).
--
-- It is named like this so that we can use it like so :
--
-- @
-- connect (net \"GND\") (pin "R1" 1)
-- @
--
-- which is quite convenient :)
pin ::  String      -- ^ Reference of the component
        -> Int      -- ^ Pin number
        -> Circuit  -- ^ Circuit to look in
        -> String
pin fpRef pinNumber c = pinNetName
  where
    fp = case filter (\fp -> getFpRef fp == fpRef) $ toListOf (_footprints . traverse) c of
            [] -> error $ "Component " ++ fpRef ++ " doesn't exist"
            [x] -> x
            _ -> error $ "Multiple components with same reference (" ++ fpRef ++ ") found "
    pin = case filter (\p -> padNumber p == pinNumber ) $ toListOf (_fpContent . _fpElements . traverse . _pad) fp of
            [] -> error $ "No pin " ++ show pinNumber ++ " in component " ++ fpRef
            [x] -> x
            _ -> error $ "Multiple pins found for component " ++ fpRef ++ "and pin " ++ show pinNumber
    pinNetName = netName $ (\p -> padNet p) pin

-- | Generates a function which returns the same net name for evercy Circuit
-- passed as argument.
--
-- We define it so that we can use 'connect' like so :
--
-- @
-- connect (net \"GND\") (pin "R1" 1)
-- @
--
-- which is also quite convenient :)
net ::  String
        -> Circuit -> String
net = const

-- | Replaces occurences of Net name 2 by Net name 1
-- Beware, connect s1 s2 is different from connect s2 s1
connect ::  (Circuit -> String)       -- ^ Function returning net name 1
            -> (Circuit -> String)    -- ^ Function returnin net name 2
            -> Circuit                -- ^ Circuit where the connection is made
            -> Circuit
connect f1 f2 c = c2
  where
    nn1 = f1 c  -- nn is for "net name"
    nn2 = f2 c
    f (Net nn) | nn == nn2 = Net nn1
    f (Net nn) = Net nn
    f (NumberedNet _ _) = error $ "Cannot connect a numbered net (nn1 = " ++ nn1 ++ " nn2 = " ++ nn2 ++")"
    c1 = over (_footprints . traverse . _fpContent . _fpElements . traverse . _pad . _net) f c
    c2 = over (_segments . traverse . _segNet) f c1
