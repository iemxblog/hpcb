module Hpcb.Data.KicadPCB (
  kicadPCB
) where

import Hpcb.SExpr
import Hpcb.Data.Circuit
import Hpcb.Data.NetNumbering
import Hpcb.Data.Net

-- | Generates the general section from a given circuit.
general :: Circuit -> Item
general c@(Circuit f g s) = Item "general" [
  Item "links" [PInt 0],
  Item "no_connects" [PInt 0],
  Item "area" [PInt 0, PInt 0, PInt 0, PInt 0],
  Item "thickness" [PFloat 1.6],
  Item "drawings" [PInt drawings],
  Item "tracks" [PInt tracks],
  Item "zones" [PInt 0],
  Item "modules" [PInt modules],
  Item "nets" [PInt nets]
  ]
  where
    drawings = length g
    tracks = length s
    modules = length f
    nets = length $ listOfNets c

-- | Generic layer list.
layerList :: Item
layerList = Item "layers" [
  Item "0" [PString "F.Cu", PString "signal"],
  Item "31" [PString "B.Cu", PString "signal"],
  Item "32" [PString "B.Adhes", PString "user"],
  Item "33" [PString "F.Adhes", PString "user"],
  Item "34" [PString "B.Paste", PString "user"],
  Item "35" [PString "F.Paste", PString "user"],
  Item "36" [PString "B.SilkS", PString "user"],
  Item "37" [PString "F.SilkS", PString "user"],
  Item "38" [PString "B.Mask", PString "user"],
  Item "39" [PString "F.Mask", PString "user"],
  Item "40" [PString "Dwgs.User", PString "user"],
  Item "41" [PString "Cmts.User", PString "user"],
  Item "42" [PString "Eco1.User", PString "user"],
  Item "43" [PString "Eco2.User", PString "user"],
  Item "44" [PString "Edge.Cuts", PString "user"],
  Item "45" [PString "Margin", PString "user"],
  Item "46" [PString "B.CrtYd", PString "user"],
  Item "47" [PString "F.CrtYd", PString "user"],
  Item "48" [PString "B.Fab", PString "user"],
  Item "49" [PString "F.Fab", PString "user"]
  ]

-- | Generic setup section.
setup :: Item
setup = Item "setup" [
  Item "last_trace_width" [PFloat 0.1524],
  Item "trace_clearance" [PFloat 0.1524],
  Item "zone_clearance" [PFloat 0.508],
  Item "zone_45_only" [PString "no"],
  Item "trace_min" [PFloat 0.1524],
  Item "segment_width" [PFloat 0.2],
  Item "edge_width" [PFloat 0.15],
  Item "via_size" [PFloat 0.6858],
  Item "via_drill" [PFloat 0.3302],
  Item "via_min_size" [PFloat 0.6858],
  Item "via_min_drill" [PFloat 0.3302],
  Item "uvia_size" [PFloat 0.762],
  Item "uvia_drill" [PFloat 0.508],
  Item "uvias_allowed" [PString "no"],
  Item "uvia_min_size" [PFloat 0],
  Item "uvia_min_drill" [PFloat 0],
  Item "pcb_text_width" [PFloat 0.3],
  Item "pcb_text_size" [PFloat 1.5, PFloat 1.5],
  Item "mod_edge_width" [PFloat 0.15],
  Item "mod_text_size" [PInt 1, PInt 1],
  Item "mod_text_width" [PFloat 0.15],
  Item "pad_size" [PFloat 1.524, PFloat 1.524],
  Item "pad_drill" [PFloat 0.762],
  Item "pad_to_mask_clearance" [PFloat 0.2],
  Item "aux_axis_origin" [PInt 0, PInt 0],
  Item "visible_elements" [PString "FFFFFF7F"],
  Item "pcbplotparams" [
    Item "layerselection" [PString "0x00030_80000001"],
    Item "usegerberextensions" [PString "false"],
    Item "excludeedgelayer" [PString "true"],
    Item "linewidth" [PFloat 0.1],
    Item "plotframeref" [PString "false"],
    Item "viasonmask" [PString "false"],
    Item "mode" [PInt 1],
    Item "useauxorigin" [PString "false"],
    Item "hpglpennumber" [PInt 1],
    Item "hpglpenspeed" [PInt 20],
    Item "hpglpendiameter" [PInt 15],
    Item "hpglpenoverlay" [PInt 2],
    Item "psnegative" [PString "false"],
    Item "psa4output" [PString "false"],
    Item "plotreference" [PString "true"],
    Item "plotvalue" [PString "true"],
    Item "plotinvisibletext" [PString "false"],
    Item "padonsilk" [PString "false"],
    Item "subtractmaskfromsilk" [PString "false"],
    Item "outputformat" [PInt 1],
    Item "mirror" [PString "false"],
    Item "drillshape" [PInt 1],
    Item "scaleselection" [PInt 1],
    Item "outputdirectory" [PString "\"\""]
    ]
  ]

nets :: Circuit -> [Item]
nets c = nl
  where Item _ nl = itemize $ netsMap c

-- | Builds the default net class.
netClass :: Circuit -> Item
netClass c = Item "net_class" ([
  PString "Default",
  PString "\"Ceci est la Netclass par défaut\"",
  Item "clearance" [PFloat 0.1524],
  Item "trace_width" [PFloat 0.1524],
  Item "via_drill" [PFloat 0.3302],
  Item "uvia_dia" [PFloat 0.762],
  Item "uvia_drill" [PFloat 0.508]
  ] ++ nl
  )
  where
    nl = map (\n -> Item "add_net" [PString . show $ netName n]) $ listOfNets c

-- | Transform a circuit into the S-Expression format.
kicadPCB :: Circuit   -- ^ Circuit to transform
            -> Item
kicadPCB c@(Circuit f g s) = Item "kicad_pcb" ([
  Item "version" [PInt 4],
  Item "host" [PString "pcbnew", PString "4.0.5+dfsg1-4"],
  general c,
  Item "page" [PString "A4"],
  layerList,
  setup

  ]
  ++ nets c
  ++ [netClass c]
  ++ map itemize f
  ++ map itemize g
  ++ map itemize s
  )
