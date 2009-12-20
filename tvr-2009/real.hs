-- real numbers as staged intervals

module Real where

import Control.Monad.Reader
import Space
import Interval

data Real = Real {Reader Size Interval}

withIntervals f x y = do i <- x
                         j <- y
                         return $ f x y

instance Hasudorff Real where
  neq x y = do i <- x
               j <- y
               return $ toSemi (upper i < lower j || ) 