{- | This module defines the interval domain, i.e., the space of
  intervals. Actually, what we define as a /base/ for such a domain
  because our intervals have rational endpoints (to be exact, the
  endpoints are elements of an approximate field). The actual interval
  domains is defined in the module "Reals".
-}

module Interval where

import Space
import Staged
import Dyadic
import Debug.Trace

trace' :: String -> a -> a
-- trace' = trace
trace' _ todo = todo


{- | An interval is represented by a lower and upper endpoint. We do
  /not/ require that the lower endpoint be smaller or equal to the
  upper one. In other words, we allow the usual as well as
  back-to-front intervals. This is useful in certain kinds of
  computations.

  A possible optimization: represent an interval with its center point
  and radius, where the radius is not a precise number (has a small
  mantissa). This can save up to 50% of space, but it is not clear how
  to treat back-to-front intervals then. Presumably with negative
  radii, except I have never worked out how to implement interval
  multiplication then. -}

data Interval q = Interval { lower :: q, upper :: q }

instance ApproximateField q => Show (Interval q) where
  show Interval{lower=a, upper=b} =
    if a == b
    then "[" ++ show a ++ ",==]"
    else "[" ++ show a ++ "," ++ show b ++ "]"  

class ApproximateField q => IntervalDomain q  where
  iless :: Stage -> Interval q -> Interval q -> Bool
  imore :: Stage -> Interval q -> Interval q -> Bool
  iadd :: Stage -> Interval q -> Interval q -> Interval q
  isub :: Stage -> Interval q -> Interval q -> Interval q
  imul :: Stage -> Interval q -> Interval q -> Interval q
  iinv :: Stage -> Interval q -> Interval q
  idiv :: Stage -> Interval q -> Interval q -> Interval q
  iabs :: Stage -> Interval q -> Interval q
  inormalize :: Stage -> Interval q -> Interval q
  embed :: Stage -> q -> Interval q
  split :: Interval q -> (Interval q, Interval q)
  invert :: Interval q -> Interval q
  width :: Stage -> Interval q -> q

{-
  |- The relation `less` for two reals approximated by the two given intervals.
  iless i@Interval{lower=i1,upper=i2} j@Interval{lower=j1,upper=j2} = 
    --Staged $ \s -> 
    --trace' ("           iless called with "++(show i)++"  "++(show j)) $ 
    case (i1<=i2, j1<=j2) of
      (True, True)  
        | i2<j1 -> PTrue
        | i1>=j2 -> PFalse
        | otherwise -> PBottom
      (False, False)
        | i1<j2 -> PTrue
        | i2>=j1 -> PFalse
        | otherwise -> PTop
      (True, False)
        | i2<j1  && i1>=j2 -> PTop
        | i2>=j1 && i1<j2  -> PBottom
        | i2<j1            -> PTrue
        | i2>=j1           -> PFalse
      (False, True)
        | j2>i1  && j1<=i2 -> PBottom
        | j2<=i1 && j1>i2  -> PTop
        | j2>i1            -> PTrue
        | j2<=i1           -> PFalse
-}

  -- XXX (OK) - Za input [1,3]<[2,4] bi moral RoundUp vrniti true, ker lahko pri vecji natancnosti iz tega nastane [1,1]<[2,2] -- NAROBE! to je RoundUp, tako da bojo v prihodnjosti intervali samo bolj posploseni)      
  iless s Interval{lower=a,upper=b} Interval{lower=c,upper=d} = let 
    i = Interval{lower=normalize s a, upper=normalize (anti s) b}
    j = Interval{lower=normalize s c, upper=normalize (anti s) d}
    in
     trace' ((show i)++"  <  "++(show j)++"   -->   "++(show (upper i < lower j))) $ upper i < lower j
  
  imore s i j = iless s j i  

  iadd s a b = Interval { lower = app_add s (lower a) (lower b),
                          upper = app_add (anti s) (upper a) (upper b)}

  isub s a b = Interval { lower = app_sub s (lower a) (upper b),
                          upper = app_sub (anti s) (upper a) (lower b)}

  -- Kaucher multiplication
  imul s Interval{lower=a,upper=b} Interval{lower=c,upper=d} =
    let negative q = (compare q zero == LT)
        lmul = app_mul s
        umul = app_mul (anti s)
    in Interval { lower = (if negative a
                           then if negative b
                                then if negative d
                                     then lmul b d
                                     else lmul a d
              	                else if negative c
              	                     then if negative d 
              	                          then lmul b c
              	                          else min (lmul a d) (lmul b c)
              	                     else if negative d
              	                          then zero
              	                          else lmul a d
              	           else if negative b
              	                then if negative c
              	                     then if negative d
              	                          then lmul b d
              	                          else zero
              	                     else if negative d
              	                          then max (lmul a c) (lmul b d)
              	                          else lmul a c
              	           else if negative c
              	                then lmul b c
              	                else lmul a c),
  	              upper = (if negative a
  	                       then if negative b
  	                            then if negative c
  	                                 then umul a c
  	                                 else umul b c
                            	  else if negative c
                            	       then if negative d
                            	            then umul a c
                            	            else max (umul a c) (umul b d)
                            	       else if negative d
                            	            then zero
                            	            else umul b d
                           else if negative b
                                then if negative c
                                     then if negative d
                                          then umul a d
                                          else zero
                            	       else if negative d
                            	            then min (umul a d) (umul b c)
                            	            else umul b c
                            	  else if negative d
                            	       then umul a d
                            	       else umul b d)}

  iinv s Interval{lower=a, upper=b} =
    let sgn q = compare q zero
        linv = app_inv s
        uinv = app_inv (anti s)
    in Interval { lower = (case (sgn a, sgn b) of
                        	   (LT, LT) -> linv b
                        	   (EQ, LT) -> linv b
                        	   (GT, LT) -> positive_inf
                        	   (LT, EQ) -> negative_inf
                        	   (EQ, EQ) -> negative_inf
                        	   (GT, EQ) -> positive_inf
                        	   (LT, GT) -> negative_inf
                        	   (EQ, GT) -> negative_inf
                        	   (GT, GT) -> linv b),
                  upper = (case (sgn a, sgn b) of
                        	   (LT, LT) -> uinv a
                        	   (EQ, LT) -> negative_inf
                        	   (GT, LT) -> negative_inf
                        	   (LT, EQ) -> positive_inf
                        	   (EQ, EQ) -> positive_inf
                        	   (GT, EQ) -> uinv a
                        	   (LT, GT) -> positive_inf
                        	   (EQ, GT) -> positive_inf
                        	   (GT, GT) -> uinv a)}

  idiv s a b = imul s a (iinv s b)
  
  inormalize s a = Interval { lower = normalize s (lower a),
                              upper = normalize (anti s) (upper a) }

  embed s q = Interval { lower = q, upper = q }

  iabs s Interval{lower=a, upper=b} =
    let sgn p = compare p zero
        q = app_negate s a
        r = app_negate s b
        i = Interval { lower = (case (sgn a, sgn b) of
                                    (LT, LT) -> if q < r then q else r
                                    (GT, GT) -> if a < b then a else b
                                    (_, _)   -> zero),
                       upper = (case (sgn a, sgn b) of
                                    (EQ, LT) -> r
                                    (LT, EQ) -> q
                                    (EQ, _)  -> b
                                    (_, EQ)  -> a
                                    (LT, LT) -> if q < r then r else q
                                    (GT, GT) -> if a < b then b else a
                                    (GT, LT) -> if a < r then r else a
                                    (LT, GT) -> if q < b then b else q)}
    in case rounding s of
        RoundDown -> i
        RoundUp   -> invert i


  split Interval {lower=a, upper=b} =
    let c = midpoint a b
    in (Interval {lower=a, upper=c}, Interval {lower=c, upper=b})
    
  invert Interval {lower=a, upper=b} = Interval {lower=b, upper=a}
  
  width s Interval {lower=a, upper=b} = app_sub s b a
