{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- Basic definitions od spaces and their properties

module Space where

import Staged

-- Staged Booleans give semidecidable values

type SBool = Staged Bool

sor :: SBool -> SBool -> SBool
sor = lift2 (\s p q -> p || q)

sand :: SBool -> SBool -> SBool
sand = lift2 (\s p q -> p && q)

force :: SBool -> Bool
force p = loop 0
          where loop k = case approximate p (prec RoundDown k) of
                           True  -> True
                           False -> case approximate p (prec RoundUp k) of
                                      False -> False
                                      True  -> loop (k+1)
   
-- Properties of subspaces

class Hausdorff t where
  apart :: t -> t -> SBool 

class Discrete t where
  equal :: t -> t -> SBool
  
class Compact s t | s -> t where
  forall :: s -> (t -> SBool) -> SBool

class Overt s t | s -> t where
  exists :: s -> (t -> SBool) -> SBool
