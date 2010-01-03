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

withIntervals :: (Stage -> Interval q -> Interval q -> Interval q) -> (RealNum q -> RealNum q -> RealNum q)
withIntervals f x y = do i <- x
                         j <- y
                         s <- get_stage
                         return $ f s i j

withInterval :: (Stage -> Interval q -> Interval q) -> (RealNum q -> RealNum q)
withInterval f x = do i <- x
                      s <- get_stage
                      return $ f s i

instance IntervalDomain q => Hausdorff (RealNum q) where
  apart x y = do i <- x
                 j <- y
                 return $ semi (lt i j || lt j i)

instance IntervalDomain q => Eq (RealNum q) where
  -- | It is a very bad idea to use equality on the real numbers, since
  -- all you can ever hope for is to get @False@ or non-termination. But
  -- Haskell wants equality on numerical types, so here it is.
  x == y = force (fmap not) (x `apart` y)
  x /= y = force id (x `apart` y)
  
instance IntervalDomain q => Ord (RealNum q) where
  -- Comparison never return @EQ@, but can return @LT@ and @GT@
  compare x y = force
                (\(i,j) -> if lt i j
                           then Just LT
                           else if lt j i
                                then Just GT
                                else Nothing)
                (do { i <- x; j <- y; return (i, j)})
    
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

-- Topological properties of reals

newtype ClosedInterval q = ClosedInterval (q, q)

instance IntervalDomain q => Compact (ClosedInterval q) (RealNum q) where
  forall (ClosedInterval(a,b)) p = chain c
    where c k = case s k of
                  [] -> Just True
                  lst -> loop lst where loop [] = Nothing
                                        loop ((_, Just False) : lst) = Just False
                                        loop (_ : lst) = loop lst
          s 0 = f 0 [] [Interval {lower=a, upper=b}]
          s k = f k [] $ halve (s (k-1)) 
          f k acc [] = acc
          f k acc (j:js) = case approximate (p (chain $ const j)) k of
                             Just False -> [(j, Just False)]
                             Just True -> f k acc js
                             Nothing -> f k ((j, Nothing):acc) js
          halve [] = []
          halve ((j,_):js) = let (j1,j2) = split j in j1 : j2 : halve js
