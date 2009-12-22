module Staged where
    
import Maybe

-- A monad for staged computation

data Staged a = Staged (Int -> a)
run n (Staged s) = s n

instance Monad Staged where
	return x = Staged $ \n -> x
	s >>= f = Staged  $ \n -> run n (f (run n s))

stage = Staged $ \n -> n

instance Functor Staged where
    fmap f s = Staged $ \n -> f (run n s)

fmap2 :: (a -> b -> c) -> Staged a -> Staged b -> Staged c
fmap2 f s t = Staged $ \n -> f (run n s) (run n t)

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

-- Staged partial Booleans

type SBool = Staged PBool

sor :: SBool -> SBool -> SBool
sor = fmap2 por

sand :: SBool -> SBool -> SBool
sand = fmap2 pand

snot :: SBool -> SBool
snot = fmap pnot


