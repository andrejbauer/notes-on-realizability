{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- Basic definitions od spaces and their properties

module Space where

import Staged

-- Basic spaces

-- Partial Booleans

type PBool = Maybe Bool

por :: PBool -> PBool -> PBool
(Just True)   `por` q            = Just True
(Just False)  `por` q            = q
p             `por` (Just True)  = Just True
p             `por` (Just False) = p
Nothing       `por` Nothing      = Nothing

pand :: PBool -> PBool -> PBool
(Just True)   `pand` q            = q
(Just False)  `pand` q            = Just False
p             `pand` (Just True)  = p
p             `pand` (Just False) = Just False
Nothing       `pand` Nothing      = Nothing

pnot :: PBool -> PBool
pnot = fmap not

semi :: Bool -> PBool
semi False = Nothing
semi True  = Just True

-- Staged partial Booleans

type SBool = Staged PBool

fmap2 :: (a -> b -> c) -> Staged a -> Staged b -> Staged c
fmap2 f x y = do u <- x
                 v <- y
                 return $ f u v

sor :: SBool -> SBool -> SBool
sor = fmap2 por

sand :: SBool -> SBool -> SBool
sand = fmap2 pand

snot :: SBool -> SBool
snot = fmap pnot

-- Properties of subspaces

class Hausdorff t where
  apart :: t -> t -> SBool 

class Discrete t where
  equal :: t -> t -> SBool
  
class Compact s t | s -> t where
  forall :: s -> (t -> SBool) -> SBool

class Overt s t | s -> t where
  exists :: s -> (t -> SBool) -> SBool
