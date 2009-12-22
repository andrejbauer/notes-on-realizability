-- Basic definitions od spaces and their properties

module Space where

import Control.Monad.Reader

data RoundingMode = RoundUp | RoundDown
                  deriving Show

anti_round RoundUp   = RoundDown
anti_round RoundDown = RoundUp

data Stage = Stage { stage :: Int, rounding :: RoundingMode }
             deriving Show

anti s = Stage {stage = stage s, rounding = anti_round (rounding s)}
down s = Stage {stage = stage s, rounding = RoundDown}
up s   = Stage {stage = stage s, rounding = RoundUp}

prec k = Stage {stage = k, rounding = RoundDown}

type Staged t = Reader Stage t

compute :: Int -> Staged t -> t
compute k x = runReader x (prec k)


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

-- embedding of booleans as Sierpinski in partial booleans
toSemi :: Bool -> PBool
toSemi True = Just True
toSemi False = Nothing

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

toBool :: SBool -> Bool
toBool p = loop (prec 0)
           where loop s = case runReader p s of
                              Nothing -> loop (Stage {stage = stage s+1, rounding = rounding s})
                              Just b -> b

-- Properties of spaces

class Hausdorff s where
  neq :: s -> s -> PBool
  
class Discrete s where
  eq :: s -> s -> PBool

class Compact s where
  forall :: (s -> PBool) -> PBool
  
class Overt s where
  exists :: (s -> PBool) -> PBool
