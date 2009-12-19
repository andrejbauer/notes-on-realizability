-- The interval domain with back-to-front intervals

module Interval where

import Dyadic
import Control.Monad.Reader

data Interval a = Interval { lower :: Dyadic, upper :: Dyadic }
                  deriving (Eq, Show)

instance Ord Interval where
  compare i j = compare (upper i) (lower j)

instance Num Interval where
  (Interval {lower=il, upper=iu}) + (Interval {lower=jl, upper=ju}) = Interval {lower = il + jl, upper = iu + ju}

  (Interval {lower=il, upper=iu}) - (Interval {lower=jl, upper=ju}) = Interval {lower = il - jl, upper = iu - ju}

  -- Kaucher multiplication
  (Interval {lower=il, upper=iu}) * (Interval {lower=jl, upper=ju}) = Interval {lower = kl, upper = ku}
      where kl =
            ku = 

data IntervalMode = IntervalMode { size :: Int }

class StagedEq a where
  (==) :: a -> a -> SBool
  (/=) :: a -> a -> SBool
  