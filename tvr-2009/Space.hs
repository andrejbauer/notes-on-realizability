{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- Basic definitions od spaces and their properties

module Space where

import Staged

-- Basic spaces

-- The flat lattice adds bottom and top to a set

data Flat t = Bottom | Value t | Top

instance Monad Flat where
  return            = Value
  Bottom >>= f      = Bottom
  (Value x) >>= f = f x
  Top >>= f         = Top

instance Show t => Show (Flat t) where
  show Bottom = "undefined"
  show (Value x) = show x
  show Top = "overdefined"

instance Functor Flat where
  fmap f Bottom      = Bottom
  fmap f (Value x)   = Value (f x)
  fmap f Top         = Top

join :: Eq t => Flat t -> Flat t -> Flat t 
join Bottom v = v
join u Bottom = u
join Top v = v
join u Top = u
join (Value x) (Value y) | x == y    = Value x
                         | otherwise = Top

meet :: Eq t => Flat t -> Flat t -> Flat t 
meet Bottom v = v
meet u Bottom = u
meet Top v = v
meet u Top = u
meet (Value x) (Value y) | x == y    = Value x
                         | otherwise = Bottom

combine :: Stage -> (u -> v -> w) -> Flat u -> Flat v -> Flat w
combine s f x y =
  case (rounding s, x, y) of
    (_, Bottom, Bottom) -> Bottom
    (_, Bottom, Value _) -> Bottom
    (RoundDown, Bottom, Top) -> Bottom
    (RoundUp, Bottom, Top) -> Top
    (_, Value _, Bottom) -> Bottom
    (_, Value x, Value y) -> Value (f x y)
    (_, Value _, Top) -> Top
    (RoundDown, Top, Bottom) -> Bottom
    (RoundUp, Top, Bottom) -> Top
    (_, Top, Value _) -> Top
    (_, Top, Top) -> Top
  
-- Partial Booleans with bottom and top

type PBool = Flat Bool

por :: Stage -> PBool -> PBool -> PBool
por s = combine s (||)

pand :: Stage -> PBool -> PBool -> PBool
pand s = combine s (&&)

pnot :: Stage -> PBool -> PBool
pnot s = fmap not

-- semi :: Bool -> PBool
-- semi False = Bottom
-- semi True  = Value True

-- Staged partial Booleans

type SBool = Staged Bool

sor :: SBool -> SBool -> SBool
sor = lift2 por

sand :: SBool -> SBool -> SBool
sand = lift2 pand

snot :: SBool -> SBool
snot = lift1 pnot

-- Properties of subspaces

class Hausdorff t where
  apart :: t -> t -> SBool 

class Discrete t where
  equal :: t -> t -> SBool
  
class Compact s t | s -> t where
  forall :: s -> (t -> SBool) -> SBool

class Overt s t | s -> t where
  exists :: s -> (t -> SBool) -> SBool
