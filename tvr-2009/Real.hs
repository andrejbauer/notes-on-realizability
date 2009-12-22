-- real numbers as staged intervals

module Real where

import Control.Monad.Reader
import Dyadic
import Space
import Interval

data CauchyReal d q = Real {approx :: Staged d}

withIntervals f x y = do i <- approx x
                         j <- approx y
                         s <- ask
                         return $   f s x y

instance IntervalDomain d q => Show (CauchyReal d q) where
  show x = "<real>"

instance IntervalDomain d q => Eq (CauchyReal d q) where
  x == y = error "not implemented"
  x /= y = error "not implemented"
  
instance IntervalDomain d q => Ord (CauchyReal d q) where
  compare x y = error "not implemnted"

instance IntervalDomain d q => Num (CauchyReal d q) where
  x + y = Real (withIntervals iadd x y)
  x - y = Real (withIntervals isub x y)
  x * y = Real (withIntervals imul x y)
  
