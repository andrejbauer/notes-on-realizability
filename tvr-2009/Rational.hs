-- A type class for rational numbers, appropriate for interval arithmetic

module Rational where

import Control.Monad.Reader
import Data.Bits

data RoundingMode = RoundUp | RoundDown

opposite :: RoundingMode -> RoundingMode
opposite RoundUp = RoundDown
opposite RoundDown = RoundUp

oppositeIfNegative r x =
    if signum x < 0 then opposite r else r

data Dyadic = Dyadic { mant :: Integer, expo :: Int }
              deriving Show
              
data Mode = Mode { size :: Int, roundingMode :: RoundingMode }

-- See http://www.haskell.org/pipermail/haskell-cafe/2008-February/039640.html
ilogb :: Integer -> Integer -> Int
ilogb b n | n < 0      = ilogb b (- n)
          | n < b      = 0
          | otherwise  = (up b n 1) - 1
  where up b n a = if n < (b ^ a)
                      then bin b (quot a 2) a
                      else up b n (2*a)
        bin b lo hi = if (hi - lo) <= 1
                         then hi
                         else let av = quot (lo + hi) 2
                              in if n < (b ^ av)
                                    then bin b lo av
                                    else bin b av hi

shift_with_round r k x =
    let y = shiftR x k
    in case r of
        RoundDown -> if signum y > 0 then y else succ y
        RoundUp -> if signum y > 0 then succ y else y

normalize x =
    let m = ilogb 2 (mant x)
    in do n <- asks size
          r <- asks roundingMode
          return $  if m < n
                    then return x
                    else return $ Dyadic { mant = shift_with_round r (m-n) (mant x),
                                           expo = expo x + (m-n) }

add_raw (Dyadic {mant=m1, expo=e1}) (Dyadic {mant=m2, expo=e2}) =
    Dyadic { mant = (if e1 < e2 then m1 + shiftL m2 (e2 - e1) else shiftL m1 (e1 - e2) + m2),
             expo = min e1 e2 }

add x y = normalize (add_raw x y)
