module Math.CF.Internal where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (intersperse)
import Data.Ratio

data CF = CF [Integer] deriving (Eq)

instance Show CF where
  show (CF xs) =
    "[" ++ show (fromMaybe 0 (listToMaybe xs)) ++
    "; " ++ concat (intersperse ", " (map show (drop 1 xs))) ++ "]"

toCF' :: Integral a => (a, a) -> [a] -> [a]
toCF' (n, d) acc =
  if d == 0
  then acc
  else let
    q = n `div` d
    s = (n - (q * d))
    in toCF' (d, s) (q:acc)

toCF :: (Integer, Integer) -> CF
toCF n = CF (reverse $ toCF' n [])

fromCF :: Fractional a => CF -> a
fromCF (CF []) = 0
fromCF (CF (x:xs)) = fromIntegral x + (1/fromCF (CF xs))
