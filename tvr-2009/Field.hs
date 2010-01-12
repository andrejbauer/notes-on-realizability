{- | This module contains class defintions related to the notion of real numbers.
-}
module Field where

import Staged
import Space

{- | An approximate field is a structure in which we can perform approximate
arithmetical operations. The typical example is the ring of dyadic rational
numbers: division of dyadic rationals is only approximate, and even though the
other operations (+, -, *) can be peformed exactly, it is too expensive and
unecessary to do so in an interval computation. Therefore, we want approximate
versions of all operations.

The approximate operations take a 'Stage' argument which tells whether the
result of the operation should be rounded up or down, in the sense of the
linear ordering of the structure, and how precise the result should be.

(Missing explanation of what exactly an approximate field is supposed to be.)
-}
class (Show q, Ord q) => ApproximateField q where
  normalize :: Stage -> q -> q
  size :: q -> Int -- ^ the size of the number (memory usage)
  log2 :: q -> Int -- ^ @log2 q@ is a number @k@ such that @2^k <= abs q <= 2^(k+1)@.

  midpoint :: q -> q -> q -- ^ exact midpoint

  zero :: q
  positive_inf :: q
  negative_inf :: q

  app_add :: Stage -> q -> q -> q
  app_sub :: Stage -> q -> q -> q
  app_mul :: Stage -> q -> q -> q
  app_inv :: Stage -> q -> q
  app_div :: Stage -> q -> q -> q
  app_negate :: Stage -> q -> q
  app_abs :: Stage -> q -> q
  app_signum :: Stage -> q -> q
  app_fromInteger :: Stage -> Integer -> q
  app_shift :: Stage -> q -> Int -> q -- ^ shift by a power of 2