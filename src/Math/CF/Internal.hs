module Math.CF.Internal where

import Data.Ratio

data CF a = CF a [a] deriving (Eq, Show)

toCF' :: Integral a => (a, a) -> [a] -> [a]
toCF' (n, d) acc =
  if d == 0
  then acc
  else let
    q = n `div` d
    s = (n - (q * d))
    in toCF' (d, s) (q:acc)

toCF :: Integral a => (a, a) -> CF a
toCF n = CF (head cf) (tail cf)
  where
    cf = reverse $ toCF' n []

fromCF :: Fractional a => CF a -> a
fromCF (CF i []) = i
fromCF (CF i (x:xs)) = x + (1/fromCF (CF i xs))
