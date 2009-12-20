{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- Dyadic rational numbers, appropriate for interval arithmetic

module Dyadic where

import Control.Monad.Reader
import Data.Bits

-- There are implemented in Haskell with Integer. A faster implementation
-- would use hmpfr.

data Dyadic = Dyadic { mant :: Integer, expo :: Int }
            | PositiveInfinity
            | NegativeInfinity
            deriving Show -- TODO: Write a better Show instance

withMantissas :: (Integer -> Integer -> a) -> Dyadic -> Dyadic -> a
withMantissas f (Dyadic {mant=m1, expo=e1}) (Dyadic {mant=m2, expo=e2}) =
  if e1 < e2
  then f m1 (shiftR m2 (e2-e1))
  else f (shiftR m1 (e2-e1)) m2

-- zeroCmp q returns the same thing as compare 0 q
zeroCmp :: Dyadic -> Ordering
zeroCmp NegativeInfinity = GT
zeroCmp PositiveInfinity = LT
zeroCmp Dyadic {mant=m, expo=e} = compare 0 m

instance Eq Dyadic where
  PositiveInfinity == PositiveInfinity = True
  NegativeInfinity == NegativeInfinity = True
  a@(Dyadic _ _)   == b@(Dyadic _ _)   = withMantissas (==) a b
  _                == _                = False

  PositiveInfinity /= PositiveInfinity = False
  NegativeInfinity /= NegativeInfinity = False
  a@(Dyadic _ _)   /= b@(Dyadic _ _)   = withMantissas (/=) a b
  _                /= _                = True

instance Ord Dyadic where
  compare NegativeInfinity NegativeInfinity = EQ
  compare NegativeInfinity _                = LT
  compare _                NegativeInfinity = GT
  compare PositiveInfinity PositiveInfinity = EQ
  compare PositiveInfinity _                = GT
  compare _                PositiveInfinity = LT
  compare a@(Dyadic _ _)   b@(Dyadic _ _)   = withMantissas compare a b

instance Num Dyadic where
  -- addition
  NegativeInfinity + PositiveInfinity = error "NegativeInfinity + PositiveInfinity"
  PositiveInfinity + NegativeInfinity = error "PositiveInfinity + NegativeInfinity"
  NegativeInfinity + _ = NegativeInfinity
  _ + NegativeInfinity = NegativeInfinity
  PositiveInfinity + _ = PositiveInfinity
  _ + PositiveInfinity = PositiveInfinity
  Dyadic {mant=m1, expo=e1} + Dyadic {mant=m2, expo=e2} = Dyadic {mant = m3, expo = e3}
      where m3 = if e1 < e2 then m1 + shiftL m2 (e2 - e1) else shiftL m1 (e1 - e2) + m2
            e3 = min e1 e2

  -- subtraction
  NegativeInfinity - NegativeInfinity = error "NegativeInfinity - NegativeInfinity"
  PositiveInfinity - PositiveInfinity = error "PositiveInfinity - PositiveInfinity"
  NegativeInfinity - _ = NegativeInfinity
  _ - NegativeInfinity = PositiveInfinity
  PositiveInfinity - _ = PositiveInfinity
  _ - PositiveInfinity = NegativeInfinity
  Dyadic {mant=m1, expo=e1} - Dyadic {mant=m2, expo=e2} = Dyadic {mant = m3, expo = e3}
      where m3 = if e1 < e2 then m1 + shiftL m2 (e2 - e1) else shiftL m1 (e1 - e2) + m2
            e3 = min e1 e2

  -- multiplication
  NegativeInfinity * q = case zeroCmp q of
                           LT -> NegativeInfinity -- 0 < q
                           EQ -> fromInteger 0    -- 0 == q
                           GT -> PositiveInfinity -- q < 0
  PositiveInfinity * q = case zeroCmp q of
                           LT -> PositiveInfinity -- 0 < q
                           EQ -> fromInteger 0    -- 0 == q
                           GT -> NegativeInfinity -- q < 0
  q@(Dyadic _ _) * NegativeInfinity = NegativeInfinity * q
  q@(Dyadic _ _) * PositiveInfinity = PositiveInfinity * q
  Dyadic {mant=m1, expo=e1} * Dyadic {mant=m2, expo=e2} = Dyadic {mant = m1 * m2, expo = e1 + e2}

  -- absolute value
  abs PositiveInfinity = PositiveInfinity
  abs NegativeInfinity = NegativeInfinity
  abs Dyadic {mant=m, expo=e} = Dyadic {mant = abs m, expo = e}
  
  -- signum
  signum PositiveInfinity = fromInteger 1
  signum NegativeInfinity = fromInteger (-1)
  signum Dyadic {mant=m, expo=e} = fromInteger (signum m)

  -- fromInteger
  fromInteger i = Dyadic {mant = i, expo = 0}
    
shift :: Dyadic -> Int -> Dyadic
shift Dyadic {mant=m, expo=e} k = Dyadic {mant = m, expo = e + k}

-- dyadics with normalization and rounding form an "approximate" field in which
-- operations can be performed up to a given precision

data RoundingMode = RoundUp | RoundDown

anti RoundUp   = RoundDown
anti RoundDown = RoundUp

type Size = Int -- measure of space consumption, e.g., approximate size of mantissa

class (Show q, Ord q) => ApproximateField q where
  add :: RoundingMode -> Size -> q -> q -> q
  sub :: RoundingMode -> Size -> q -> q -> q
  mul :: RoundingMode -> Size -> q -> q -> q
  div :: RoundingMode -> Size -> q -> q -> q
  int :: RoundingMode -> Size -> Integer -> q
  normalize :: RoundingMode -> Size -> q -> q


instance ApproximateField Dyadic where
  -- We take the easy route: first we perform an exact operation then we normalize the result.
  -- A better implementation would directly compute the approximation, but it's probably not
  -- worth doing this with Dyadics. If you want speed, use hmpfr.
  add r k a b = normalize r k (a + b)
  sub r k a b = normalize r k (a - b)
  mul r k a b = normalize r k (a * b)
  int r k i   = normalize r k (fromInteger i)

  div r k a = error "div is not implemented"

  normalize r k PositiveInfinity = PositiveInfinity
  normalize r k NegativeInfinity = NegativeInfinity
  normalize r k a@(Dyadic {mant=m, expo=e}) =
      let j = ilogb 2 m
      in  if j <= k
          then a
          else Dyadic {mant = shift_with_round r (j-k) m, expo = e + (j-k) }


-- See http://www.haskell.org/pipermail/haskell-cafe/2008-February/039640.html
ilogb :: Integer -> Integer -> Int
ilogb b n | n < 0      = ilogb b (- n)
          | n < b      = 0
          | otherwise  = (up b n 1) - 1
  where up b n a = if n < (b ^ a)
                      then bin b (quot a 2) a
                      else up b n (2*a)
        bin b lo hi = if (hi - lo) <= 1
                         then hi
                         else let av = quot (lo + hi) 2
                              in if n < (b ^ av)
                                    then bin b lo av
                                    else bin b av hi

shift_with_round r k x =
    let y = shiftR x k
    in case r of
        RoundDown -> if signum y > 0 then y else succ y
        RoundUp -> if signum y > 0 then succ y else y
