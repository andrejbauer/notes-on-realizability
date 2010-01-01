{-# LANGUAGE TypeSynonymInstances #-}
-- real numbers as staged intervals

module CReal where

import Ratio
import Staged
import Space
import Dyadic
import Interval
import Field

type RealNum q = Staged (Interval q)

withIntervals :: (Stage -> Interval q -> Interval q -> Interval q) -> (RealNum q -> RealNum q -> RealNum q)
withIntervals f x y = do i <- x
                         j <- y
                         s <- get_stage
                         return $ f s i j

withInterval :: (Stage -> Interval q -> Interval q) -> (RealNum q -> RealNum q)
withInterval f x = do i <- x
                      s <- get_stage
                      return $ f s i

instance ApproximateField q => Eq (RealNum q) where
  x == y = error "== not implemented"
  x /= y = error "/= not implemented"
  
instance ApproximateField q => Ord (RealNum q) where
  compare x y = error "compare not implemented"

instance (ApproximateField q, IntervalDomain q) => Num (RealNum q) where
    x + y = withIntervals iadd x y
    x - y = withIntervals isub x y
    x * y = withIntervals imul x y
  
    abs x = withInterval iabs x

    signum x = do i <- x
                  s <- get_stage
                  return $ Interval { lower = app_signum s (lower i),
                                      upper = app_signum (anti s) (upper i) }

    fromInteger k = do s <- get_stage
                       return $ Interval { lower = app_fromInteger s k,
                                           upper = app_fromInteger (anti s) k }

instance (ApproximateField q, IntervalDomain q) => Fractional (RealNum q) where
    x / y = withIntervals idiv x y

    recip x = withInterval iinv x

    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance IntervalDomain Dyadic

exact :: RealNum Dyadic -> RealNum Dyadic
exact x = x