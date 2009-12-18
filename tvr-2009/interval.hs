-- The interval domain with back-to-front intervals

module Interval where

import Rational
    
data Interval a = Interval { lower :: Field a, upper :: Field a }

add :: Size -> Interval a -> Interval a -> Interval a
add k i j = Interval { lower = add k RoundDown (lower i) (lower j),
                       upper = add k RoundUp (upper i) (upper j) }
