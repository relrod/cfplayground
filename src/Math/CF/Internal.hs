{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.CF.Internal where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (intersperse)
import Data.Ratio
import Data.Semigroup

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

-- | Convert from a 'CF' to a different representation, potentially subject to
-- rounding errors and other evilness.
unsafeFromCF :: (Integral a, Fractional b) => CF a -> b
unsafeFromCF (CF []) = 0
unsafeFromCF (CF (x:xs)) = fromIntegral x + (1/unsafeFromCF (CF xs))

-- | For any invertable 2x2 matrix, we can obtain a Mobius transformation
-- (specifically a homographic function) of the form @h(z) = (az + b)/(cz + d)@.
data Mobius a = Mobius a a
                       a a deriving (Eq, Functor, Show)

instance Num a => Semigroup (Mobius a) where
  Mobius x1 x2 x3 x4 <> Mobius y1 y2 y3 y4 =
    Mobius (x1 * y1 + x2 * y3)
           (x2 * y1 + x2 * y3)
           (x3 * y2 + x3 * y4)
           (x4 * y2 + x4 * y4)

instance Num a => Monoid (Mobius a) where
  mappend = (<>)
  mempty = Mobius 1 0 0 1   -- Identity 2x2 matrix

-- | Determinant of a 'Mobius' representation
determinant :: Num a => Mobius a -> a
determinant (Mobius x1 x2 x3 x4) = x1 * x4 - x2 * x3
