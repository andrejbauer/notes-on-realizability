{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}
-- real numbers as staged intervals

module CReal where

import Ratio
import Staged
import Space
import Dyadic
import Interval
import Field

type RealNum q = Staged (Interval q)

-- Comparison

less :: IntervalDomain q => RealNum q -> RealNum q -> SBool
less = lift2 (\_ -> iless)

more :: IntervalDomain q => RealNum q -> RealNum q -> SBool
more x y = less y x

-- Properties of equality

instance IntervalDomain q => Hausdorff (RealNum q) where
  apart x y = less x y `sor` less y x

instance IntervalDomain q => Eq (RealNum q) where
  -- | It is a very bad idea to use equality on the real numbers, since
  -- all you can ever hope for is to get @False@ or non-termination. But
  -- Haskell wants equality on numerical types, so here it is.
  x == y = not $ force $ x `apart` y
  x /= y = force $ x `apart` y
  
instance IntervalDomain q => Ord (RealNum q) where
  -- Comparison never return @EQ@, but can return @LT@ and @GT@
  compare x y = case force (x `less` y) of
                  True  -> LT
                  False -> GT
    
instance (ApproximateField q, IntervalDomain q) => Num (RealNum q) where
    x + y = lift2 iadd x y
    x - y = lift2 isub x y
    x * y = lift2 imul x y
  
    abs x = lift1 iabs x

    signum x = do i <- x
                  s <- get_stage
                  return $ Interval { lower = app_signum s (lower i),
                                      upper = app_signum (anti s) (upper i) }

    fromInteger k = do s <- get_stage
                       return $ Interval { lower = app_fromInteger s k,
                                           upper = app_fromInteger (anti s) k }

instance (ApproximateField q, IntervalDomain q) => Fractional (RealNum q) where
    x / y = lift2 idiv x y

    recip x = lift1 iinv x

    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance IntervalDomain Dyadic

exact :: RealNum Dyadic -> RealNum Dyadic
exact x = x

-- Compactness of closed intervals

newtype ClosedInterval q = ClosedInterval (q, q)

instance IntervalDomain q => Compact (ClosedInterval q) (RealNum q) where
  forall (ClosedInterval(a,b)) p =
    chain (\s ->
      let r = rounding s
          n = precision s
          test_interval u v = case r of
                                RoundDown -> Interval {lower = u, upper = v}
                                RoundUp   -> let w = midpoint u v in Interval {lower = w, upper = w}
          sweep [] = True
          sweep ((k,a,b):lst) = let x = chain $ const (test_interval a b)
                                   in case (r, approximate (p x) (prec r k)) of
                                     (RoundDown, False) -> (k < n) &&
                                                           (let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)]))
                                     (RoundDown, True)  -> sweep lst
                                     (RoundUp,   False) -> False
                                     (RoundUp,   True)  -> (k >= n) ||
                                                           (let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)]))                                     
      in sweep [(0,a,b)]
    )
