module Main (
  main
) where

import Hpcb
import Data.Monoid

vline :: Float -> Circuit
vline l = (
  line (V2 0 0) (V2 0 (-l)) # layer FCu
  <> line (V2 0 0) (V2 0 (-l)) # layer FMask
  ) # width 0.15

tree :: Int           -- ^ Depth of the tree
        -> Float      -- ^ Length of the trunk
        -> Float      -- ^ Length of the branches
        -> Float      -- ^ Angle of the branches
        -> Float      -- ^ Reduction factor
        -> Circuit
tree 0 _ _ _ _ = mempty
tree n l lb a f =
  vline l
  <> branch # rotate (-a) # translate (V2 0 (-l))
  <> branch # rotate a # translate (V2 0 (-l))
  where
    branch =
      vline lb
      <> tree (n-1) (l*f) (lb*f) a f # translate (V2 0 (-lb))

main :: IO ()
main = runCircuit $
  tree 10 10 5 20 0.75
  <> rectangle 70 60 # layer EdgeCuts # translate (V2 0 (-27))
