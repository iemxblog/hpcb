module Main (
  main
) where

import Hpcb
import Data.Monoid

r :: Int -> String
r n = "R" ++ show n

-- | Generates a r2r ladder with the desired number of bits
r2r_ladder ::   Int      -- ^ Number of bits
                -> Circuit
r2r_ladder n = cc4

  where
    components =
      pinHeader "JP1" 1 (n+2)
      <> mconcat [ r805 (r i) "20k" # translate (V2 5.08 (2.54 * fromIntegral (i-1))) | i <- [1..n]]
      <> mconcat [ r805 (r (i+n)) "10k" # translate (V2 10.16 (2.54 * fromIntegral (i-1))) | i <- [1..n-1]]
      <> r805 (r (2*n)) "20k" # translate (V2 5.08 (2.54 * fromIntegral n))
    cc1 = foldr (\i c -> connect (pin "JP1" i) (pin (r i) 1) c) components [1..n]
    cc2 = foldr (\i c -> connect (pin (r i) 2) (pin (r (i+n)) 1) c) cc1 [1..n-1]
    cc3 = foldr (\i c -> connect (pin (r (i+n)) 2) (pin (r (i+1)) 2) c) cc2 [1..n-1]
    cc4 =
      cc3
      # connect (pin (r n) 2) (pin (r (2*n)) 2)
      # connect (net "GND") (pin (r (2*n)) 1)
      # connect (net "GND") (pin "JP1" (n+2))
      # connect (net "VOUT") (pin "JP1" (n+1))
      # connect (net "VOUT") (pin (r 1) 2)

main :: IO ()
main = runCircuit $ r2r_ladder 8
