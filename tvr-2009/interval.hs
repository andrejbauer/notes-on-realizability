-- The interval domain with back-to-front intervals

module Interval where

import Dyadic
import Control.Monad.Reader

data Interval q = Interval { lower :: q, upper :: q }
                  deriving Show

instance ApproximateField q => Eq (Interval q) where
  i == j = (lower i == lower j) && (upper i == upper j)
  i == j = (lower i /= lower j) || (upper i /= upper j)
  
instance ApproximateField q => Ord (Interval q) where
  compare i j = compare (upper i) (lower j)

instance ApproximateField q => Num (Interval q) where
  