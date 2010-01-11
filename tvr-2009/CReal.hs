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

less x y = do i <- x
              j <- y
              return $ case (i `iless` j, j `iless` i) of
                         (False, False) -> Bottom
                         (True,  False) -> Value True
                         (False, True)  -> Value False
                         (True, True)   -> Top

more x y = less y x

-- Properties of equality

instance IntervalDomain q => Hausdorff (RealNum q) where
  apart x y = less x y `sor` less y x

instance IntervalDomain q => Eq (RealNum q) where
  -- | It is a very bad idea to use equality on the real numbers, since
  -- all you can ever hope for is to get @False@ or non-termination. But
  -- Haskell wants equality on numerical types, so here it is.
  x == y = force (\x -> case x of { Value x -> Just (not x) ; _ -> Nothing}) (x `apart` y)
  x /= y = force (\x -> case x of { Value x -> Just x ; _ -> Nothing}) (x `apart` y)
  
instance IntervalDomain q => Ord (RealNum q) where
  -- Comparison never return @EQ@, but can return @LT@ and @GT@
  compare x y = force
                (\p -> case p of { Value False -> Just GT ; Value True -> Just LT ; _ -> Nothing})
                (x `less` y)
    
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
    chain (\n -> sweep n (Just True) [(0,Interval{lower=a,upper=b})])
    where sweep n b [] = b
          sweep n b ((k,j):lst) =
            case approximate (p (chain $ const j)) k of
              Just False -> Just False -- short-circuit, we found a counter-example
              Just True -> sweep n b lst
              Nothing -> if k < n
                         then let (j1,j2) = split j in sweep n b (lst ++ [(k+1,j1), (k+1,j2)])
                         else sweep n Nothing lst