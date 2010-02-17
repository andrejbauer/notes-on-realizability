module ApproximateFloating (
  ApproximateFloating(..), 
) where

import Staged

class (Show q, Ord q, Fractional q) => ApproximateFloating q where
  app_pi :: Stage -> q
  app_exp :: Stage -> q -> q
  app_log :: Stage -> q -> q
  app_sqrt :: Stage -> q -> q
  app_pow :: Stage -> q -> q -> q
  app_logBase :: Stage -> q -> q -> q
  app_sin :: Stage -> q -> q
  app_cos :: Stage -> q -> q
  app_tan :: Stage -> q -> q
  app_asin :: Stage -> q -> q
  app_acos :: Stage -> q -> q
  app_atan :: Stage -> q -> q
  app_sinh :: Stage -> q -> q
  app_cosh :: Stage -> q -> q
  app_tanh :: Stage -> q -> q
  app_asinh :: Stage -> q -> q
  app_acosh :: Stage -> q -> q
  app_atanh :: Stage -> q -> q
  -- Minimal complete definition:
  app_logBase s x y = (app_log s y) / (app_log s x)
        --      pi, exp, log, sin, cos, sinh, cosh
        --      asinh, acosh, atanh
  -- x ** y           =  exp (log x * y)
  -- logBase x y      =  log y / log x
  -- sqrt x           =  x ** 0.5
  -- tan  x           =  sin  x / cos  x
  -- tanh x           =  sinh x / cosh x
