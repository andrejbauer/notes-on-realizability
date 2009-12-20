-- The interval domain with back-to-front intervals

module Interval where

import Dyadic

data Interval q = Interval { lower :: q, upper :: q }
                  deriving Show

class Eq i => IntervalDomainBase i where
  cmp : i -> i -> Maybe Ordering
  add : RoundingMode -> Size -> i -> i -> i
  sub : RoundingMode -> Size -> i -> i -> i
  mul : RoundingMode -> Size -> i -> i -> i
  div : RoundingMode -> Size -> i -> i -> i
  exact : 

instance ApproximateField q => Eq (Interval q) where
  i == j = (lower i == lower j) && (upper i == upper j)
  i /= j = (lower i /= lower j) || (upper i /= upper j)
  

instance ApproximateField q => ApproximateField (Interval q) where
  add r k a b = Interval { lower = add r k (lower a) (lower b),
                           upper = add (anti r) k (upper a) (upper b)}

  sub r k a b = Interval { lower = sub r k (lower a) (lower b),
                           upper = sub (anti r) k (upper a) (upper b)}

  -- Kaucher multiplication
  mul r k a b = error "multiplication not implemented"
  
  div r k a b = error "division not implemented"
  
  int r k i = Interval { lower = int r k i, upper = int (anti r) k i }
  
  normalize r k a = Interval { lower = normalize r k (lower a),
                               upper = normalize (anti r) k (upper a) }
