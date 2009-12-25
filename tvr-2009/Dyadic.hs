{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

-- Dyadic rational numbers, appropriate for interval arithmetic

module Dyadic where

import Data.Bits

import Staged

-- There are implemented in Haskell with Integer. A faster implementation
-- would use hmpfr.

data Dyadic = Dyadic { mant :: Integer, expo :: Int }
            | PositiveInfinity
            | NegativeInfinity

instance Show Dyadic where
    show Dyadic {mant=m, expo=e} =
        (show $ encodeFloat m e) ++ " [m=" ++ (show m) ++ ", e=" ++ (show e) ++ "]"

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
      where m3 = if e1 < e2 then m1 - shiftL m2 (e2 - e1) else shiftL m1 (e1 - e2) - m2
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

class (Show q, Ord q) => ApproximateField q where
  add :: Stage -> q -> q -> q
  sub :: Stage -> q -> q -> q
  mul :: Stage -> q -> q -> q
  quo :: Stage -> q -> q -> q
  neg :: q -> q
  absolute :: Stage -> q -> q
  sgn :: Stage -> q -> q
  int :: Stage -> Integer -> q
  normalize :: Stage -> q -> q

instance ApproximateField Dyadic where
  -- We take the easy route: first we perform an exact operation then we normalize the result.
  -- A better implementation would directly compute the approximation, but it's probably not
  -- worth doing this with Dyadics. If you want speed, use hmpfr.
  add s a b = normalize s (a + b)
  sub s a b = normalize s (a - b)
  mul s a b = normalize s (a * b)
  neg   a   = negate a
  absolute s a = normalize s (abs a)
  sgn s a = normalize s (signum a)

  int s i   = normalize s (fromInteger i)


  quo s Dyadic{mant=m1,expo=e1} Dyadic{mant=m2,expo=e2} =
      let e = stage s
          r = case rounding s of
                RoundDown -> 0
                RoundUp -> 1
      in Dyadic {mant = r + (shiftL 1 e * m1) `div` m2, expo = e1 - e2 - e}

  normalize s PositiveInfinity = PositiveInfinity
  normalize s NegativeInfinity = NegativeInfinity
  normalize s a@(Dyadic {mant=m, expo=e}) =
      let j = ilogb 2 m
          k = stage s
          r = rounding s
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
