{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

{- | We implement real numbers as the completion of dyadic intervals. The whole construction is
   parametrized by an approximate field, an example of which is "Dyadic".
-}

module Reals where

import Ratio
import Staged
import Space
import Dyadic
import Interval


-- | A real number is implemented as a staged dyadic interval @'Interval' q@ where @q@ is the
-- underlying approximate field (in practice these are dyadic rationals). @'RealNum' q@ can be used
-- to represent not only real numbers but also the elements of the interval domain, including the
-- back-to-front intervals.
type RealNum q = Staged (Interval q)

-- | We implement a very simple show instance for reals which computes the 20th approximation
-- and shows it as an interval, together with a floating point approximation.
instance ApproximateField q => Show (RealNum q) where
   show x = let i = approximate x (prec RoundDown 20)
            in show i ++ " " ++ show (toFloat (midpoint (lower i) (upper i)))

-- | Linear order on real numbers
instance IntervalDomain q => LinearOrder (RealNum q) where
    less = lift2 (\_ -> iless)

-- | It is a bad idea to use Haskell-style inequality @/=@ on reals because it either returns @True@
-- or it diverges. Similarly, using Haskell equality @==@ is bad. Nevertheless, we define @==@ and @/=@
-- because Haskell wants them for numeric types.
instance IntervalDomain q => Eq (RealNum q) where
    x /= y = force $ x `apart` y

-- | Real numbers are an ordered type in the sense of Haskells 'Ord', although a comparison never
-- returns @EQ@ (instead it diverges). This is a fact of life, comparison of reals is not decidable.
instance IntervalDomain q => Ord (RealNum q) where
  compare x y = case force (x `less` y) of
                  True  -> LT
                  False -> GT

-- | The ring structure for the reals.
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

-- | Division and reciprocals.
instance (ApproximateField q, IntervalDomain q) => Fractional (RealNum q) where
    x / y = lift2 idiv x y
            
    recip x = lift1 iinv x
                      
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- | The Hausdorff property
instance IntervalDomain q => Hausdorff (RealNum q) where
     x `apart` y = (x `less` y) `sor` (y `less` x)

-- | The value @ClosedInterval(a,b)@ represents the closed interval [a,b] as a subspace of the reals.
newtype ClosedInterval q = ClosedInterval (q, q)

-- | Compactness of the closed interval
instance IntervalDomain q => Compact (ClosedInterval q) (RealNum q) where
   forall (ClosedInterval(a,b)) p =
     limit (\s ->
       let r = rounding s
           n = precision s
           test_interval u v = case r of
                                 RoundDown -> Interval {lower = u, upper = v}
                                 RoundUp   -> let w = midpoint u v in Interval {lower = w, upper = w}
           sweep [] = True
           sweep ((k,a,b):lst) = let x = return $ test_interval a b
                                    in case (r, approximate (p x) (prec r k)) of
                                      (RoundDown, False) -> (k < n) &&
                                                            (let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)]))
                                      (RoundDown, True)  -> sweep lst
                                      (RoundUp,   False) -> False
                                      (RoundUp,   True)  -> (k >= n) ||
                                                            (let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)]))
       in sweep [(0,a,b)]
     )

-- | Missing: overtness of reals, open interval (a,b) and closed interval [a,b]
instance IntervalDomain q => Overt (ClosedInterval q) (RealNum q) where
     exists (ClosedInterval (a,b)) p = error "Not implemented"

-- | We define a particular implementation of reals in terms of Dyadic numbers. Because 'IntervalDomain' has
-- a default implementation for all of its components we don't have to implement anything.
instance IntervalDomain Dyadic

-- | This is a convenience function which allows us to write @exact 1.3@ as a conversion from floating points to
-- real numbers. There probably is a better way of doing this.
exact :: RealNum Dyadic -> RealNum Dyadic
exact x = x

-- MISSING STRUCTURE:
-- Metric completeness: an operator lim which takes a Cauchy sequence and its convergence rate, and computes the limit
-- Density of rationals: given a real x and integer k, compute a dyadic approximation of a real x which is within 2^(-k)a
