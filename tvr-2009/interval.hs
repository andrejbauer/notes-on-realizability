{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- The interval domain with back-to-front intervals

module Interval where

import Space
import Dyadic

class ApproximateField q => IntervalDomain d q | d -> q where
  lower :: d -> q
  upper :: d -> q
  lt :: d -> d -> Bool
  iadd :: Stage -> d -> d -> d
  isub :: Stage -> d -> d -> d
  imul :: Stage -> d -> d -> d
  idiv :: Stage -> d -> d -> d
  inormalize :: Stage -> d -> d
  embed :: Stage -> q -> d

data Interval q = Interval { low :: q, high :: q }
                  deriving Show

instance ApproximateField q => IntervalDomain (Interval q) q where
  lower = low
  
  upper = high

  lt i j = high i < low j

  iadd s a b = Interval { low = add s (low a) (low b),
                          high = add (anti s) (high a) (high b)}

  isub s a b = Interval { low = sub s (low a) (low b),
                          high = sub (anti s) (high a) (high b)}

  -- Kaucher multiplication
  imul s a b = error "multiplication not implemented"
  
  idiv s a b = error "division not implemented"
    
  inormalize s a = Interval { low = normalize s (low a),
                              high = normalize (anti s) (high a) }

  embed s q = Interval { low = normalize s q,
                         high = normalize (anti s) q }
