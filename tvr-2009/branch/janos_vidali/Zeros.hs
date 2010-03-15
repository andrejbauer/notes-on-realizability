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
                let lucky x = let xi = approximate x stg
                              in lower xi == 0 && lower xi == upper xi
                    real x = Staged (\st -> Interval.embed st x)
                    std = prec_down $ precision stg
                    lmin x y = let lx = upper $ approximate x std
                                   ly = upper $ approximate y std
                               in min lx ly
                    lmax x y = let lx = lower $ approximate x std
                                   ly = lower $ approximate y std
                               in max lx ly
                    l0 = lower int
                    u0 = upper int
                    fl0 = f $ real l0
                    fu0 = f $ real u0
                    sl = fl0 < 0
                    su = fu0 < 0
                    ii = case l0 < u0 of
                            True -> int
                            False -> invert int
                    mf = case sl of
                            True  -> max
                            False -> min
                    db x = real $ mf x 0
                    bisect s i = let r = rounding s
                                     p = precision s
                                     l = lower i
                                     u = upper i
                                     m = midpoint l u
                                     fm = f $ real m
                                     sm = fm < 0
                                     (li, ui) = split i
                                 in if p == 0 || l == u
                                     then case r of
                                         RoundDown -> i
                                         RoundUp   -> invert i
                                    else if lucky fm
                                     then Interval.embed s m
                                    else if sl == sm
                                     then newton s ui
                                     else newton s li
                    newton s i = let r = rounding s
                                     p = precision s
                                     ns = prec r (p-1)
                                     l = lower i
                                     u = upper i
                                     rl = real l
                                     ru = real u
                                     fl = f rl
                                     fu = f ru
                                     dn = approximate (d (Staged (\_ -> i))) std
                                     ld = db $ lower dn
                                     ud = db $ upper dn
                                     ll = rl - fl/ld
                                     lu = rl - fl/ud
                                     ul = ru - fu/ud
                                     uu = ru - fu/ld
                                     ni = case sl of
                                            True  -> Interval {lower = lmax lu uu,
                                                               upper = lmin ul ll}
                                            False -> Interval {lower = lmax ll ul,
                                                               upper = lmin uu lu}
                                 in bisect ns ni
                in case (lucky fl0, lucky fu0, sl == su) of
                   (True, _, _) -> Interval.embed stg l0
                   (_, True, _) -> Interval.embed stg u0
                   (_, _, True) -> error "The function must have different signs on the edges of the interval!"
                   (_, _, _)    -> bisect stg ii
            )

-- | Find zero with bisection. The derivative fed to the 'FindZero' function
-- is actually the constant function 'unbounded', which gives no information
-- about the slope to the Newton method, so the bisection actually does all
-- the work.
findZeroBisection :: (RealNum Dyadic -> RealNum Dyadic) -> Interval Dyadic -> RealNum Dyadic
findZeroBisection f i = findZero f unbounded i