{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

{- | We implement real numbers as the completion of dyadic intervals. The whole construction is
   parametrized by an approximate field, an example of which is "Dyadic".
-}

module Reals where

import Ratio
import Staged
import Space
import Dyadic
import Mpfr
import Interval
import ApproximateField
import ApproximateFloating


-- | A real number is implemented as a staged dyadic interval @'Interval' q@ where @q@ is the
-- underlying approximate field (in practiec these are dyadic rationals). @'RealNum' q@ can be used
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

-- | The ring structure fo the reals.
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


instance (ApproximateFloating q, IntervalDomain q) => Floating (RealNum q) where
    pi = do 
            s <- get_stage
            return $ Interval { lower = app_pi s, upper = app_pi (anti s) }     
    
    exp x = do 
            i <- x
            s <- get_stage
            return $ Interval { lower = app_exp s (lower i), upper = app_exp (anti s) (upper i) }
    
    log x = do 
            i <- x
            s <- get_stage
            return $ Interval { lower = if (lower i) > 0 then app_log s (lower i) else negative_inf, 
                                upper = if (upper i) > 0 then app_log (anti s) (upper i) else negative_inf }
    
	-- TODO sin cos
    sin x = do
            i <- x
            s <- get_stage
            let d = app_abs s (app_sub s (upper i) (lower i))
            return $ (if d < 2 * (app_pi s)
						then Interval { lower = -1, upper = 1 }
						else Interval { lower = -1, upper = 1 })
	
    sqrt x = do 
            i <- x
            s <- get_stage
            return $ Interval { lower = if (lower i) > 0 then app_sqrt s (lower i) else 0, 
                                upper = if (upper i) > 0 then app_sqrt (anti s) (upper i) else 0 }
										
    asin x = do
            i <- x
            s <- get_stage
            return $ Interval { lower = if (lower i) > (-1)
											then if (lower i) < 1 
												then app_asin s (lower i)
												else (app_pi s) / 2
										else (app_pi (anti s)) / (-2),
								upper = if (upper i) > (-1)
											then if (upper i) < 1 
												then app_asin (anti s) (upper i)
												else (app_pi (anti s)) / 2
										else (app_pi s) / (-2) }
										
    acos x = do
            i <- x
            s <- get_stage
            return $ Interval { lower = if (lower i) > (-1)
											then if (lower i) < 1 
												then app_acos s (upper i)
												else 0
										else (app_pi (anti s)) * (-1),
								upper = if (upper i) > (-1)
											then if (upper i) < 1 
												then app_acos (anti s) (lower i)
												else 0
										else (app_pi s) * (-1) }
										
    atan x = do 
            i <- x
            s <- get_stage
            return $ Interval { lower = app_atan s (lower i), 
                                upper = app_atan (anti s) (upper i) }

    sinh x = do 
            i <- x
            s <- get_stage
            return $ Interval { lower = app_sinh s (lower i), 
                                upper = app_sinh (anti s) (upper i) }

    cosh x = do 
			i <- x
			s <- get_stage
			let sgn_l = (app_signum s (lower i))
			let sgn = sgn_l * (app_signum s (upper i))
			let m = max (app_cosh (anti s) (lower i)) (app_cosh (anti s) (upper i))
			return $ Interval { lower = if sgn > 0 
										then if sgn_l > 0
											then app_cosh s (lower i)
											else app_cosh s (upper i)
										else 1,
								upper = if sgn > 0
										then if sgn_l > 0
											then app_cosh (anti s) (upper i)
											else app_cosh (anti s) (lower i)
										else m }

    tanh x = do 
            i <- x
            s <- get_stage
            return $ Interval { lower = app_tanh s (lower i), 
                                upper = app_tanh (anti s) (upper i) }

    asinh x = do 
            i <- x
            s <- get_stage
            return $ Interval { lower = app_asinh s (lower i), 
                                upper = app_asinh (anti s) (upper i) }

    acosh x = do 
            i <- x
            s <- get_stage
            return $ Interval { lower = if (lower i) > 1 then app_acosh s (lower i) else 0, 
                                upper = if (upper i) > 1 then app_acosh (anti s) (upper i) else 0 }


    atanh x = do
            i <- x
            s <- get_stage
            return $ Interval { lower = if (lower i) > (-1)
											then if (lower i) < 1 
												then app_atanh s (lower i)
												else positive_inf
										else negative_inf,
								upper = if (upper i) > (-1)
											then if (upper i) < 1 
												then app_atanh (anti s) (upper i)
												else positive_inf
										else negative_inf }
----------------------------------------------------------------------------------------------------------------------------------------

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

-- | We define the a particular implementation of reals in terms of Dyadic numbers. Because 'IntervalDomain' has
-- a default implementation for all of its components we don't have to implement anything.
instance IntervalDomain Dyadic

-- | This is a convenience function which allows us to write @exact 1.3@ as a conversion from floating points to
-- real numbers. There probably is a better way of doing this.
exact :: RealNum Dyadic -> RealNum Dyadic
exact x = x

instance IntervalDomain Mpfr

exac :: RealNum Mpfr -> RealNum Mpfr
exac x = x



-- MISSING STRUCTURE:
-- Metric completeness: an operator lim which takes a Cauchy sequence and its convergence rate, and computes the limit
-- Density of rationals: given a real x and integer k, compute a dyadic approximation of a real x which is within 2^(-k)a
