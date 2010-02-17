module Mpfr (
  Mpfr(..), 
) where

import ApproximateField
import ApproximateFloating

import Staged

import qualified Data.Number.MPFR as M 	-- import functions
import Data.Number.MPFR.Instances.Up 	-- import instances

data Mpfr = Mpfr {m :: M.MPFR}

instance Show Mpfr where
  show Mpfr{m=m} = show m

instance Eq Mpfr where
  Mpfr{m=m1} == Mpfr{m=m2} = m1 == m2
  Mpfr{m=m1} /= Mpfr{m=m2} = m1 /= m2

instance Ord Mpfr where
  compare Mpfr{m=m1} Mpfr{m=m2} = compare m1 m2

instance Num Mpfr where
  Mpfr{m=m1} + Mpfr{m=m2} = Mpfr (m1 + m2)
  Mpfr{m=m1} - Mpfr{m=m2} = Mpfr (m1 - m2)
  Mpfr{m=m1} * Mpfr{m=m2} = Mpfr (m1 * m2)
  abs Mpfr{m=m1} = Mpfr (abs m1)
  signum Mpfr{m=m1} = Mpfr (signum m1)
  fromInteger i = Mpfr (fromInteger i)
  
instance Fractional Mpfr where
  Mpfr{m=m1} / Mpfr{m=m2} = Mpfr (m1 / m2)
  fromRational r = Mpfr (fromRational r)
  
instance ApproximateField Mpfr where
  normalize _ m = m		-- TODO
  size _ = 0 			-- NOT IMPLEMENTED
  log2 _ = 0 			-- NOT IMPLEMENTED

  midpoint Mpfr{m=m1} Mpfr{m=m2} = if (M.isInfinite m1) && (M.isInfinite m2) && (M.isNaN (m1 + m2))
									then Mpfr M.zero
									else Mpfr ((m1 + m2) / 2)
  -- TODO
  --midpoint NegativeInfinity Dyadic{mant=m, expo=e} = Dyadic {mant = -1 - abs m, expo= 2 * max 1 e}
  --midpoint PositiveInfinity Dyadic{mant=m, expo=e} = Dyadic {mant = 1 + abs m, expo= 2 * max 1 e}
  --midpoint Dyadic{mant=m,expo=e} NegativeInfinity = Dyadic {mant = -1 - abs m, expo= 2 * max 1 e}
  --midpoint Dyadic{mant=m,expo=e} PositiveInfinity = Dyadic {mant = 1 + abs m, expo= 2 * max 1 e}
  
  zero = Mpfr M.zero
  positive_inf = Mpfr (M.setInf 100 1)		-- Mpfr (1 / 0)
  negative_inf = Mpfr (M.setInf 100 (-1))	-- Mpfr (-1 / 0)

  toFloat Mpfr{m=m} = M.toDouble M.Up m

  -- approximate operations
  app_add Stage{precision=p,rounding=r} Mpfr{m=m1} Mpfr{m=m2} = Mpfr (M.add (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m1 m2)
  app_sub Stage{precision=p,rounding=r} Mpfr{m=m1} Mpfr{m=m2} = Mpfr (M.sub (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m1 m2)
  app_mul Stage{precision=p,rounding=r} Mpfr{m=m1} Mpfr{m=m2} = Mpfr (M.mul (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m1 m2)
  app_div Stage{precision=p,rounding=r} Mpfr{m=m1} Mpfr{m=m2} = Mpfr (M.div (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m1 m2)
  app_negate Stage{precision=p,rounding=r} Mpfr{m=m}          = Mpfr (M.neg (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_abs Stage{precision=p,rounding=r} Mpfr{m=m}             = Mpfr (M.absD (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_shift Stage{precision=p,rounding=r} Mpfr{m=m} i         = Mpfr (M.mul2i (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m i)
  app_signum _ Mpfr{m=m}                                      = Mpfr (case (M.sgn m) of Just i -> (fromInteger $ toInteger i); Nothing -> M.setNaN 1)
  app_inv s m = app_div s (Mpfr 1) m
  app_fromInteger s i = Mpfr (fromInteger i) -- TODO
  
instance ApproximateFloating Mpfr where
  app_pi Stage{precision=p,rounding=r}                       = Mpfr (M.pi (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p))
  app_exp Stage{precision=p,rounding=r} Mpfr{m=m}            = Mpfr (M.exp (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_log Stage{precision=p,rounding=r} Mpfr{m=m}            = Mpfr (M.log (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_sqrt Stage{precision=p,rounding=r} Mpfr{m=m}           = Mpfr (M.sqrt (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_pow Stage{precision=p,rounding=r} Mpfr{m=m1} Mpfr{m=m2}= Mpfr (M.pow (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m1 m2)
  app_sin Stage{precision=p,rounding=r} Mpfr{m=m}            = Mpfr (M.sin (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_cos Stage{precision=p,rounding=r} Mpfr{m=m}            = Mpfr (M.cos (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_tan Stage{precision=p,rounding=r} Mpfr{m=m}            = Mpfr (M.tan (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_asin Stage{precision=p,rounding=r} Mpfr{m=m}           = Mpfr (M.asin (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_acos Stage{precision=p,rounding=r} Mpfr{m=m}           = Mpfr (M.acos (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_atan Stage{precision=p,rounding=r} Mpfr{m=m}           = Mpfr (M.atan (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_sinh Stage{precision=p,rounding=r} Mpfr{m=m}           = Mpfr (M.sinh (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_cosh Stage{precision=p,rounding=r} Mpfr{m=m}           = Mpfr (M.cosh (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_tanh Stage{precision=p,rounding=r} Mpfr{m=m}           = Mpfr (M.tanh (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_asinh Stage{precision=p,rounding=r} Mpfr{m=m}          = Mpfr (M.asinh (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_acosh Stage{precision=p,rounding=r} Mpfr{m=m}          = Mpfr (M.acosh (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  app_atanh Stage{precision=p,rounding=r} Mpfr{m=m}          = Mpfr (M.atanh (case r of { RoundUp -> M.Up ; RoundDown -> M.Down}) (fromInteger $ toInteger p) m)
  