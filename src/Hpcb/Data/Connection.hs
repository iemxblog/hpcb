module Hpcb.Data.Connection (
  name,
  names,
  pinName,
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

-- ^ Assigns names to a pin, given the reference of the component and pin number
name :: String      -- ^ Reference of the component
        -> Int      -- ^ The pin of which we want to change the name
        -> [String] -- ^ New names of the pin
        -> Circuit  -- ^ Circuit to look into
        -> Circuit
name ref num ns = over (_footprints . traverse) f
  where
    f fp = if getFpRef fp == ref
            then over (_fpContent . _fpElements . traverse . _pad) assignNames fp
            else fp
    assignNames p@(Pad pnum _ _ _ _ _ _ _) | pnum == num = p {padNames = ns}
    assignNames p = p

-- | Assigns names to pins of a circuit, with an assocation list (pin number, pin names)
-- Example of usage :
--
-- @@
-- lm358n :: String
--           -> Circuit
-- lm358n ref = soic_8 ref "LM358N" # names ref [
--     (1, [\"OUTA\"]),
--     (2, [\"-INA\"]),
--     (3, [\"+INA\"]),
--     (4, [\"GND\", \"V-\"]),
--     (5, [\"+INB\"]),
--     (6, [\"-INB\"]),
--     (7, [\"OUTB\"]),
--     (8, [\"V+\"])
--   ]
-- @@
names ::  String                -- ^ Reference of the component
          -> [(Int, [String])]  -- ^ Association list (pin number, pin names)
          -> Circuit            -- ^ Circuit where the component is located
          -> Circuit
names ref assocList circuit = foldr (\(i, ss) c -> name ref i ss c) circuit assocList

-- | Looks for a footprint, given its reference
getFootprintByRef ::  String        -- ^ Footprint reference
                      -> Circuit    -- ^ Circuit to look into
                      -> Footprint
getFootprintByRef fpRef c =
  case filter (\fp -> getFpRef fp == fpRef) $ toListOf (_footprints . traverse) c of
    [] -> error $ "Component " ++ fpRef ++ " doesn't exist"
    [x] -> x
    _ -> error $ "Multiple components with same reference (" ++ fpRef ++ ") found "

-- | Returns the nets associated with pin name
-- It is named like this so that we can use it like so :
--
-- @
-- connect (net \"GND\") (pinName "U1" \"GND\")
-- @
--
-- which is quite convenient :)
pinName ::  String                  -- ^ Reference of the component
            -> String               -- ^ Name of the pin(s)
            -> Circuit -> [String]
pinName ref pn c = pinsNets
  where
    fp = getFootprintByRef ref c
    pins = case filter (\p -> pn `elem` padNames p) $ toListOf (_fpContent . _fpElements . traverse . _pad) fp of
              [] -> error $ "No pin named " ++ show pins ++ " in component " ++ ref
              xs -> xs
    pinsNets = map (netName . padNet) pins

-- | Returns the name of the net associated with the pin (identified by component reference and pin number).
--
-- It is named like this so that we can use it like so :
--
-- @
-- connect (net \"GND\") (pin \"R1\" 1)
-- @
--
-- which is quite convenient :)
pin ::  String      -- ^ Reference of the component
        -> Int      -- ^ Pin number
        -> Circuit  -- ^ Circuit to look in
        -> [String]
pin fpRef pinNumber c = pinNetName
  where
    fp = getFootprintByRef fpRef c
    pins = case filter (\p -> padNumber p == pinNumber ) $ toListOf (_fpContent . _fpElements . traverse . _pad) fp of
            [] -> error $ "No pin " ++ show pinNumber ++ " in component " ++ fpRef
            xs -> xs
    pinNetName = map (netName . padNet) pins

-- | Generates a function which returns the same net name for evercy Circuit
-- passed as argument.
--
-- We define it so that we can use 'connect' like so :
--
-- @
-- connect (net \"GND\") (pin \"R1\" 1)
-- @
--
-- which is also quite convenient :)
net ::  String
        -> Circuit -> [String]
net s = const [s]

-- | Replaces occurences of Net name 2 by Net name 1
-- Beware, connect s1 s2 is different from connect s2 s1
connectNets ::  String       -- ^ Net name 1
                -> String    -- ^ Net name 2
                -> Circuit   -- ^ Circuit where the connection is made
                -> Circuit
connectNets nn1 nn2 c = c2
  where
    f (Net nn) | nn == nn2 = Net nn1
    f (Net nn) = Net nn
    f (NumberedNet _ _) = error $ "Cannot connect a numbered net (nn1 = " ++ nn1 ++ " nn2 = " ++ nn2 ++")"
    c1 = over (_footprints . traverse . _fpContent . _fpElements . traverse . _pad . _net) f c
    c2 = over (_segments . traverse . _segNet) f c1

-- | Connects pins and components. To be used with 'pin', 'pinName', and 'net'.
-- The final net name is the first net of the first list. So the order of lists is important.
-- Examples of usage :
--
-- @
-- connect (net \"GND\") (pin \"R1\" 1)
-- connect (net \"GND\") (pinName \"U1\" \"GND\")
-- connect (pinName \"U1\" \"VCC\") (pinName \"U1\" \"VCC\")
-- @
--
connect ::  (Circuit -> [String])       -- ^ List of nets 1
            -> (Circuit -> [String])    -- ^ List of nets 2
            -> Circuit                  -- ^ Circuit where the connections are made
            -> Circuit
connect f1 f2 c = c2
  where
    nns1 = f1 c
    nns2 = f2 c
    newNetName = case nns1 of
      [] -> error "Cannot connect an empty list of nets"
      (x:_) -> x
    c2 = foldr (\nn cir -> connectNets newNetName nn cir) c (nns1 ++ nns2)
