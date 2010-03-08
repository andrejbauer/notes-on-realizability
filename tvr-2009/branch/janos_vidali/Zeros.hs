-- | Finding zeros of functions on staged reals.

module Zeros where

import Staged
import Space
import Dyadic
import Interval
import Reals

-- | For convenience, we define the set of all real numbers as an 'Interval'.
allReals :: Interval Dyadic
allReals = Interval { lower = NegativeInfinity, upper = PositiveInfinity }

-- | Also for convenience, the same thing as a constant function.
unbounded :: RealNum Dyadic -> RealNum Dyadic
unbounded _ = Staged $ \_ -> allReals

-- | The zero finder. The first argument is the function which should have
-- opposite signs at the edges of the interval and exactly one zero on it.
-- The second argument should be the function's derivative, or, more exactly,
-- a function that gives as the result the bounds for the slope in the chosen
-- interval. This interval is given as the third and final argument.
--
-- This function alternately performs bisection and the Newton method.
-- TODO: actually implement the Newton method:)
findZero :: (RealNum Dyadic -> RealNum Dyadic) -> (RealNum Dyadic -> RealNum Dyadic) -> Interval Dyadic -> RealNum Dyadic
findZero f d int = Staged (\stg ->
                let lu x = lower x == 0 && lower x == upper x -- TODO: simplify,
                    lucky l m u = let fl = approximate l stg  -- add precision
                                      fm = approximate m stg
                                      fu = approximate u stg
                                  in case (lu fl, lu fm, lu fu) of
                                          (True, _, _) -> Just $ lower fl
                                          (_, True, _) -> Just $ lower fm
                                          (_, _, True) -> Just $ lower fu
                                          (_, _, _)    -> Nothing
                    real x = Staged (\st -> Interval.embed st x)
                    fz s i = let r = rounding s
                                 p = precision s
                                 np = prec r (p-1)
                                 l = lower i
                                 u = upper i
                                 m = midpoint l u
                                 fl = f $ real l
                                 fm = f $ real m
                                 fu = f $ real u
                                 dn = d $ Staged (\_ -> i)
                                 sl = fl < 0
                                 sm = fm < 0
                                 su = fu < 0
                                 (li, ui) = split i
                             in case lucky fl fm fu of
                                Just x  ->  Interval.embed s x
                                Nothing ->  if p == 0 || l == u
                                                then case r of
                                                    RoundDown -> i
                                                    RoundUp   -> invert i
                                            else if sl == su
                                                then error "The function must have different signs on the edges of the interval!"
                                            else if sl == sm
                                                then fz np ui
                                                else fz np li
                in fz stg int
            )

-- | Find zero with bisection. The derivative fed to the 'FindZero' function
-- is actually the constant function 'unbounded', which gives no information
-- about the slope to the Newton method, so the bisection actually does all
-- the work.
findZeroBisection :: (RealNum Dyadic -> RealNum Dyadic) -> Interval Dyadic -> RealNum Dyadic
findZeroBisection f i = findZero f unbounded i