{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.CF.Internal where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (intersperse)
import Data.Ratio

data CF a = Integral a => CF [a]
deriving instance Eq (CF a)

instance Show a => Show (CF a) where
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

toCF :: Integral a => (a, a) -> CF a
toCF n = CF (reverse $ toCF' n [])

fromCF :: (Integral a, Fractional b) => CF a -> b
fromCF (CF []) = 0
fromCF (CF (x:xs)) = fromIntegral x + (1/fromCF (CF xs))
