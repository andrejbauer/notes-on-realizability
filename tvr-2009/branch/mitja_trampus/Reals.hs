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
import Text.Printf

import Debug.Trace
-- trace :: String -> a -> a
-- trace _ todo = todo

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
    -- XXX (OK) - kaj zares naredi lift2? Liftne funkcijo, ki dela nad Stageom in dvema spremenljivkama. Vemo
    --   lift2 :: (Completion m) => (Stage -> t -> u -> v) -> m t -> m u -> m v
    -- pri cemer je za nas vedno m=Staged. Implementacija:
    --   lift2 f x y = do {a <- x; b <- y; s <- get_stage; return $ f s a b}
    -- Zakaj imamo tukaj iless powrappan v lambdo, par vrstic nizje pa iadd&Co. niso powrappani?
    --   Ker je iless funkcija dveh argumentov (samo 2 intervala), iadd&Co. pa sprejmejo tri (stage in 2 intervala -- vsoto racunajo samo na zahtevani precision natancno)


-- | It is a bad idea to use Haskell-style inequality @/=@ on reals because it either returns @True@
-- or it diverges. Similarly, using Haskell equality @==@ is bad. Nevertheless, we define @==@ and @/=@
-- because Haskell wants them for numeric types.
instance IntervalDomain q => Eq (RealNum q) where
    x /= y = forceBool $ x `apart` y

-- | Real numbers are an ordered type in the sense of Haskells 'Ord', although a comparison never
-- returns @EQ@ (instead it diverges). This is a fact of life, comparison of reals is not decidable.
instance IntervalDomain q => Ord (RealNum q) where
  compare x y = case forceBool (x `less` y) of
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

-- | The Hausdorff property
instance IntervalDomain q => Hausdorff (RealNum q) where
     x `apart` y = (x `less` y) `por` (y `less` x)

-- | The value @ClosedInterval(a,b)@ represents the closed interval [a,b] as a subspace of the reals.
newtype ClosedInterval q = ClosedInterval (q, q) 
                         deriving Show

-- convenience -- ClosedInterval on Dyadics
cid a b = (ClosedInterval(a,b) :: ClosedInterval Dyadic)
reals = ClosedInterval(Dyadic.NegativeInfinity, Dyadic.PositiveInfinity)

-- | Compactness of the closed interval
-- instance IntervalDomain q => Compact (ClosedInterval q) (RealNum q) where
forall' (ClosedInterval(a,b)) p = 
     -- limit = Staged  -- Staged :: (Completion m) => (Stage -> t) -> m t
           limit (\s ->
                   let r = rounding s
                       n = precision s
                       test_interval u v = case r of
                                             RoundDown -> Interval {lower = u, upper = v}
                                             -- ce zaokrozujemo navzgor (radi recemo True), potem bomo namesto celega intervala preverili samo sredinsko tocko
                                             RoundUp   -> let w = midpoint u v in Interval {lower = w, upper = w}
                       test_interval' u v = Interval {lower = u, upper = v}
                       subdivide ((k,a,b):lst) = 
                         let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)])
                       sweep [] = PTrue
                       -- k je precision Stagea, s katerim racunamo, ali p velja za interval [a,b].
                       -- Malo neucinkovito je, da n, do katerega povecujemo k, povecujemo postopoma (recimo iz forceBool) in za vsak n od zacetka racunamo vseh O(2^n) intervalov, namesto da bi uporabili delne rezultate od n-1.
                       -- Ni pa ta neucinkovitost prevec grozna; teh delnih rezultatov (od n-1) je samo pol toliko, kolikor jih rabimo pri n, tako da razred zahtevnosti ostane isti.
                       sweep intervals@((k,a,b):lst) = 
                         -- XXX - bi morali tukaj a, b normaliziati? Najbrz ne. Smo zelo nesrecni, ker ne moremo preveriti lastnosti p za interval [0, sqrt(3)]?
                         let x = return $ test_interval a b -- trace (show $ test_interval a b) return $ test_interval a b
                             -- approximate :: Staged (Stage -> a) -> Stage a -- samo "slece" Staged wrapper
                         in trace (printf "%20s - %-20s  --%-9s %d-->  %s" (show a) (show b) (show r) k (show $ approximate (p x) (prec r k))) $ case (r, approximate (p x) (prec r k)) of                 
                           (RoundUp,   PTop)      -> if (k>=n) then PTop else subdivide intervals
                           (RoundUp,   PBottom)   -> if (k>=n) then PTop else subdivide intervals -- PBottom je narobe. Recimo, [6,7] `less` [6.5,6.5] se z natancnostjo 2 ali vec roundupa v PBottom. Tudi PFalse je narobe; rezultat PBottom nam sicer garantira, da vsaj za eno tocko levega intervala in eno tocko desnega intervala relacija `less` (ali `more`) ne drzi, ampak pri vecjih natancnostih bodo intervali morda manjsi, CEPRAV delamo RoundUp. Recimo, [3,4] `less` [4,5] je PBottom, ampak ce preverjamo x `less` (x+1), je smiselno it v vecjo natancnost.
                           (RoundUp,   PTrue)     -> if (k >= n) 
                                                     then sweep lst -- sweep lst je narobe. Recimo [3,7] `less` [6,6] == True pri natancnosti 1 ali 0 in RoundUp-anju
                                                     else subdivide intervals
                           (RoundUp,   PFalse)    -> if (k >= n) 
                                                     then PFalse -- PFalse je narobe iz istega razloga kot zgoraj sweep lst.
                                                     else subdivide intervals                                                          
                           (RoundDown,   PTop)    -> if (k>=n) then PBottom else subdivide intervals
                           (RoundDown,   PBottom) -> if (k>=n) then PBottom else subdivide intervals 
                           (_,         PFalse)    -> if (k >= n) 
                                                      then PFalse
                                                      else subdivide intervals
                           (RoundDown,   PTrue)  -> if (k >= n) 
                                                     then sweep lst
                                                     else subdivide intervals
                       result = sweep [(0,a,b)]
                   in trace (printf "forall %s - %s v nacinu %s %d vraca %s" (show a) (show b) (show r) n (show result)) result
                 )


instance IntervalDomain q => Compact (ClosedInterval q) (RealNum q) where
   forall (ClosedInterval(a,b)) p = 
     -- limit = Staged  -- Staged :: (Completion m) => (Stage -> t) -> m t
           limit (\s ->
                   let r = rounding s
                       n = precision s
                       test_interval u v = Interval {lower = u, upper = v}
                       subdivide ((k,a,b):lst) = 
                         let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)])
                       sweep [] = PTrue
                       sweep intervals@((k,a,b):lst) = 
                         let x = return $ test_interval a b in 
--                         trace (printf "%20s - %-20s  --%-9s %d-->  %s" (show a) (show b) (show r) k (show $ approximate (p x) (prec r k))) $ 
                         case (r, approximate (p x) (prec r k), approximate (p x) (prec (anti_mode r) k)) of
                           (_,         PTrue,   PTrue)   -> sweep lst
                           (_,         PFalse,  PFalse)  -> PFalse
--                           (_,         PTop,    PTop)    -> PTop
--                           (_,         PBottom, PBottom) -> PBottom
                           (RoundUp,   PTop,    _)       -> if (k>=n) then PTop else subdivide intervals
                           (RoundUp,   PBottom, _)       -> if (k>=n) then PTop else subdivide intervals -- PBottom je narobe. Recimo, [6,7] `less` [6.5,6.5] se z natancnostjo 2 ali vec roundupa v PBottom. Tudi PFalse je narobe; rezultat PBottom nam sicer garantira, da vsaj za eno tocko levega intervala in eno tocko desnega intervala relacija `less` (ali `more`) ne drzi, ampak pri vecjih natancnostih bodo intervali morda manjsi, CEPRAV delamo RoundUp. Recimo, [3,4] `less` [4,5] je PBottom, ampak ce preverjamo x `less` (x+1), je smiselno it v vecjo natancnost.
                           (RoundUp,   PTrue,   _)       -> if (k>=n) then PTop else subdivide intervals
                           (RoundUp,   PFalse,  _)       -> if (k>=n) then PTop else subdivide intervals
                           (RoundDown, _,       _)       -> if (k>=n) then PBottom else subdivide intervals
                       result = sweep [(0,a,b)]
                   in 
--                    trace (printf "forall %s - %s v nacinu %s %d vraca %s" (show a) (show b) (show r) n (show result)) $
                    result
                 )


-- Increasing the precision grows the dyadic interval which is the upper approximation (=subinterval) of [6,7] and shrinks 
-- the negative interval which is the upper approximation (=negative superinterval) of [6.9, 6.9]. Correspondingly, the precision of the result
-- improves (PBottom being the true result)
-- let p = (\x -> x `less` 6.9); x = Staged $ \s -> Interval {lower=fromFloat 6, upper=fromFloat 7} in approximate (p x) (prec RoundUp 1)  -- PTop
-- let p = (\x -> x `less` 6.9); x = Staged $ \s -> Interval {lower=fromFloat 6, upper=fromFloat 7} in approximate (p x) (prec RoundUp 5)  -- PTrue
-- let p = (\x -> x `less` 6.9); x = Staged $ \s -> Interval {lower=fromFloat 6, upper=fromFloat 7} in approximate (p x) (prec RoundUp 50) -- PBottom

-- forall (cid 3 7) (\x -> x `less` 7.1) -- True
-- forall (cid 3 7) (\x -> x `less` (x+1)) -- True
-- forall (cid 3 7) (\x -> x `less` 6.9) -- False
-- forall (cid 3 7) (\x -> x `less` 7) -- se zacikla
-- forall (cid (-100) 100) (\x -> (x `less` 7.1) `por` (x `more` 6.9)) -- True
-- let eps=0.9 in forall (cid 0 10) (\x -> (forall (cid 0 10) (\y -> (x `less` (y+eps)) `por` (y `less` (x+eps))))) -- True
-- XXX -- trikotniska neenakost -- vrne False -- zakaj? Zaradi cudaskosti v abs?   
-- let eps=0.9 in forall (cid (-10) 10) (\x -> (forall (cid (-10) 10) (\y -> (((abs x)+(abs y)) `more` ((abs (x+y)) - eps)))))
   
-- | Overtness of the closed interval [a,b]
instance IntervalDomain q => Overt (ClosedInterval q) (RealNum q) where
   exists (ClosedInterval(a,b)) p = 
     -- limit = Staged  -- Staged :: (Completion m) => (Stage -> t) -> m t
           limit (\s ->
                   let r = rounding s
                       n = precision s
                       test_interval u v = Interval {lower = u, upper = v}
                       subdivide ((k,a,b):lst) = 
                         let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)])
                       sweep [] = PTrue
                       sweep intervals@((k,a,b):lst) = 
                         let x = return $ test_interval a b in 
--                         trace (printf "%20s - %-20s  --%-9s %d-->  %s" (show a) (show b) (show r) k (show $ approximate (p x) (prec r k))) $ 
                         case (r, approximate (p x) (prec r k), approximate (p x) (prec (anti_mode r) k)) of
                           (_,         PTrue,   PTrue)   -> sweep lst
                           (_,         PFalse,  PFalse)  -> PFalse
--                           (_,         PTop,    PTop)    -> PTop
--                           (_,         PBottom, PBottom) -> PBottom
                           (RoundUp,   PTop,    _)       -> if (k>=n) then PTop else subdivide intervals
                           (RoundUp,   PBottom, _)       -> if (k>=n) then PTop else subdivide intervals -- PBottom je narobe. Recimo, [6,7] `less` [6.5,6.5] se z natancnostjo 2 ali vec roundupa v PBottom. Tudi PFalse je narobe; rezultat PBottom nam sicer garantira, da vsaj za eno tocko levega intervala in eno tocko desnega intervala relacija `less` (ali `more`) ne drzi, ampak pri vecjih natancnostih bodo intervali morda manjsi, CEPRAV delamo RoundUp. Recimo, [3,4] `less` [4,5] je PBottom, ampak ce preverjamo x `less` (x+1), je smiselno it v vecjo natancnost.
                           (RoundUp,   PTrue,   _)       -> if (k>=n) then PTop else subdivide intervals
                           (RoundUp,   PFalse,  _)       -> if (k>=n) then PTop else subdivide intervals
                           (RoundDown, _,       _)       -> if (k>=n) then PBottom else subdivide intervals
                       result = sweep [(0,a,b)]
                   in 
--                    trace (printf "forall %s - %s v nacinu %s %d vraca %s" (show a) (show b) (show r) n (show result)) $
                    result
                 )

f x = -x*x+3*x
p x = (f x) `more` 2
r a b = Staged $ \s -> Interval {lower=fromFloat a, upper=fromFloat b}
-- let x=(r 3 3.3) in (f x)
-- Funkcija -x*x+3*x doseze max=2.25 pri x=1.5
--   exists (cid 0 10) (\x -> ((-x*x+3*x) `more` 2.24)) -- True
--   exists (cid 0 10) (\x -> ((-x*x+3*x) `more` 2.25)) -- se zacikla
-- let x = return (Interval {lower = 0, upper = 10}); p=(\x -> ((-x*x+3*x) `more` exact 2)) in approximate (p x) (prec RoundUp 190)


-- XXX - lahko v haskellu naredim inline teste? Recimo, da hocem "na tem mestu" pognati zgornji klic exists in izpisati rezultat.


-- | Missing: overtness of reals, open interval (a,b) and closed interval [a,b]


-- | We define the a particular implementation of reals in terms of Dyadic numbers. Because 'IntervalDomain' has
-- a default implementation for all of its components we don't have to implement anything.
instance IntervalDomain Dyadic

-- | This is a convenience function which allows us to write @exact 1.3@ as a conversion from floating points to
-- real numbers. There probably is a better way of doing this.
exact :: RealNum Dyadic -> RealNum Dyadic
exact x = x

-- MISSING STRUCTURE:
-- Metric completeness: an operator lim which takes a Cauchy sequence and its convergence rate, and computes the limit
-- Density of rationals: given a real x and integer k, compute a dyadic approximation of a real x which is within 2^(-k)a



-- Cudastvo
-- *Reals> normalize (prec RoundUp 1) (fromFloat 6)
-- 4*2^1(~8.0)
-- *Reals> normalize (prec RoundUp 1) (fromFloat 7)
-- 4*2^1(~8.0)
-- *Reals> normalize (prec RoundUp 1) (fromFloat 3)
-- 4*2^0(~4.0)
-- *Reals> let p = (\x -> x `less` 6); x = Staged $ \s -> Interval {lower=fromFloat 3, upper=fromFloat 7} in approximate (p x) (prec RoundUp 1)  -- PTop
-- PTrue

-- *Reals> Interval{lower=fromFloat 4, upper=fromFloat 8} `iless` Interval{lower=fromFloat 8, upper=fromFloat 8}
-- PBottom