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
-- opposite signs at the edges of the interval and a positive finite number of
-- zeroes on it. The second argument should be the function's derivative, or,
-- more exactly, a function that gives as the result the bounds for the slope in
-- the chosen interval. This interval is given as the third and final argument.
--
-- This function first performs bisection until the interval on which the zero
-- lies becomes bounded, and then alternates between the Newton method and
-- bisection. If the bisection stumbles on a midpoint for which it cannot decide
-- whether its function value is smaller than zero given the current precision,
-- it increases the precision and tries with more points, repeating the process
-- until the decision is reached for some point. This is guaranteed to happen
-- since the function only has a finite number of zeros on the given interval.
-- In this process, the points that are closest to the middle of the current
-- interval are tried first.
findZero :: (RealNum Dyadic -> RealNum Dyadic) -> (RealNum Dyadic -> RealNum Dyadic) -> Interval Dyadic -> RealNum Dyadic
findZero f d int = Staged (\stg ->
                let lucky x q = let xi = approximate x $ prec r q
                                in lower xi == 0 && lower xi == upper xi
                    real x = Staged (\st -> Interval.embed st x)
                    p = precision stg
                    r = rounding stg
                    std = prec_down p
                    lmin u x y = let lx = upper $ approximate x std
                                     ly = upper $ approximate y std
                                 in min u $ min lx ly
                    lmax l x y = let lx = lower $ approximate x std
                                     ly = lower $ approximate y std
                                 in max l $ max lx ly
                    l0 = lower int
                    u0 = upper int
                    fl0 = f $ real l0
                    fu0 = f $ real u0
                    sl = fl0 < 0 -- These two should be decidable,
                    su = fu0 < 0 -- or we wouldn't really be able to do anything.
                    ii = case l0 < u0 of
                            True  -> int
                            False -> invert int
                    mf = case sl of
                            True  -> max
                            False -> min
                    db x = real $ mf 0 x
                    double l u [ml, mu] = [ml, mu, midpoint l ml, midpoint mu u]
                    double l u (ml:mu:ms@(lm:um:_)) = let mlm = midpoint lm ml
                                                          mum = midpoint mu um
                                                      in ml:mu:mlm:mum:(double l u ms)
                    trypoint [] _ = Nothing
                    trypoint (m:ms) q = let fm = f $ real m
                                            asm = app_sigma (fm `less` 0) q
                                        in case asm of
                                            Just b  -> Just (m, fm, b, q)
                                            Nothing -> trypoint ms q
                    decpoint l u mms@(_:ms) q = let nms = double l u mms
                                                in case trypoint ms q of
                                                    Just a  -> a
                                                    Nothing -> decpoint l u nms (q+1)
                    bisect i q = let l = lower i
                                     u = upper i
                                     m0 = midpoint l u
                                     (m, fm, b, nq) = decpoint l u [m0, m0] q
                                     ni = case b == sl of
                                            True  -> Interval {lower=m, upper=u}
                                            False -> Interval {lower=l, upper=m}
                                     next = case (lower ni, upper ni) of
                                              (Dyadic _ _, Dyadic _ _) -> newton
                                              _                        -> bisect
                                 in if (width stg i) <= Dyadic {mant=1, expo=(-p)}
                                     then case r of
                                         RoundDown -> i
                                         RoundUp   -> invert i
                                    else if lucky fm nq
                                     then Interval.embed stg m
                                     else next ni nq
                    newton i q = let l = lower i
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
                                            True  -> Interval {lower = lmax l lu uu,
                                                               upper = lmin u ul ll}
                                            False -> Interval {lower = lmax l ll ul,
                                                               upper = lmin u uu lu}
                                 in bisect ni q
                in case (lucky fl0 p, lucky fu0 p, sl == su) of
                   (True, _, _) -> Interval.embed stg l0
                   (_, True, _) -> Interval.embed stg u0
                   (_, _, True) -> error "The function must have different signs on the edges of the interval!"
                   (_, _, _)    -> bisect ii p
            )

-- | Find zero with bisection. The derivative fed to the 'FindZero' function
-- is actually the constant function 'unbounded', which gives no information
-- about the slope to the Newton method, so the bisection actually does all
-- the work.
findZeroBisection :: (RealNum Dyadic -> RealNum Dyadic) -> Interval Dyadic -> RealNum Dyadic
findZeroBisection f i = findZero f unbounded i

-- findZero (\x -> (x+5)*(x-10)) (\x -> 2*x-5) Interval{lower=(-3)::Dyadic, upper=20} -- 10, OK
-- findZeroBisection (\x -> (x+5)*(x-10)) Interval{lower=(-30)::Dyadic, upper=8} -- -5, OK
