-- The interval domain with back-to-front intervals

module Interval where

import Staged | 
import Space
import Dyadic

data Interval q = Interval { lower :: q, upper :: q }
                  deriving Show

class (Num q, ApproximateField q) => IntervalDomain q  where
  lt :: Interval q -> Interval q -> Bool
  iadd :: Stage -> Interval q -> Interval q -> Interval q
  isub :: Stage -> Interval q -> Interval q -> Interval q
  imul :: Stage -> Interval q -> Interval q -> Interval q
  idiv :: Stage -> Interval q -> Interval q -> Interval q
  iabs :: Stage -> Interval q -> Interval q
  inormalize :: Stage -> Interval q -> Interval q
  embed :: Stage -> q -> Interval q
  width :: Interval q -> q

  lt i j = upper i < lower j

  iadd s a b = Interval { lower = add s (lower a) (lower b),
                          upper = add (anti s) (upper a) (upper b)}

  isub s a b = Interval { lower = sub s (lower a) (lower b),
                          upper = sub (anti s) (upper a) (upper b)}

  -- Kaucher multiplication
  imul s a b = error "multiplication not implemented"
  
  -- WARNING, this only works for positive intervals right now
  idiv s a b = Interval { lower = quo s (lower a) (upper b),
                          upper = quo (anti s) (upper a) (lower b) }
    
  inormalize s a = Interval { lower = normalize s (lower a),
                              upper = normalize (anti s) (upper a) }

  embed s q = Interval { lower = q, upper = q }

  iabs s a = Interval { lower = int s (fromInteger 0),
                        upper = let q = neg (lower a)
                                    r = upper a
                                in if q < r then r else q }

  width a = upper a - lower a