-- real numbers as staged intervals

module CReal where

import Ratio
import Control.Monad.Reader
import Dyadic
import Space
import Interval

newtype CReal q = CReal {approx :: Staged (Interval q)}

withIntervals f x y = do i <- approx x
                         j <- approx y
                         s <- ask
                         return $ f s i j

withInterval f x = do i <- approx x
                      s <- ask
                      return $ f s i

instance Show q => Show (CReal q) where
  show x = "<real>"

instance Eq q => Eq (CReal q) where
  x == y = error "== not implemented"
  x /= y = error "/= not implemented"
  
instance Ord q => Ord (CReal q) where
  compare x y = error "compare not implemented"

instance (ApproximateField q, IntervalDomain q) => Num (CReal q) where
    x + y = CReal (withIntervals iadd x y)
    x - y = CReal (withIntervals isub x y)
    x * y = CReal (withIntervals imul x y)
  
    abs x = CReal (withInterval iabs x)

    signum x = CReal (do i <- approx x
                         s <- ask
                         return $ Interval { lower = sgn s (lower i),
                                             upper = sgn (anti s) (upper i) })

    fromInteger k = CReal (do s <- ask
                              return $ Interval { lower = int s k,
                                                  upper = int (anti s) k })

instance (ApproximateField q, IntervalDomain q) => Fractional (CReal q) where
    x / y = CReal (withIntervals idiv x y)

    recip x = error "recip not implemented"

    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)



instance IntervalDomain Dyadic

fromFloat :: Float -> CReal Dyadic
fromFloat x = let (m,e) = decodeFloat x
              in  CReal (do s <- ask
                            return $ Interval { lower = Dyadic {mant = m, expo = e},
                                                upper = Dyadic {mant = m, expo = e} })
creal :: CReal Dyadic -> CReal Dyadic
creal x = x