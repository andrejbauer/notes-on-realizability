{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}

-- | Basic definitions od spaces and their properties

module Space where

import Staged
import Debug.Trace

-- | The Sierpinski space @Sigma@ is represented by staged booleans.
type Sigma = Staged Bool

-- | Disjunction for Sierpinski space
sor :: Sigma -> Sigma -> Sigma
sor = lift2 (\s p q -> p || q)

-- | Conjunction for Sierpinski space
sand :: Sigma -> Sigma -> Sigma
sand = lift2 (\s p q -> p && q)

-- | Force a value in the Sierpinski space into Booleans. This may diverge as bottom cannot be
-- reliably detected.
force :: Sigma -> Bool
force p = let (b,prec) = forceVerbose p in b

-- | Like force, but also returns the required precision to get the Bool
forceVerbose :: Sigma -> (Bool, Int)
forceVerbose p = loop 0
          where loop k = case trace "Rounding down" approximate p (prec RoundDown k) of
                           True  -> (True, k) -- lower approximation is True, the value is top
                           False -> case trace "Rounding up" approximate p (prec RoundUp k) of
                                      False -> (False, k) -- upper approximation is False, the value is bottom
                                      True  -> trace ("RoundUp/Down results disagree at precision "++(show k)) loop (k+1)

-- | *Try* to map a value in the Sierpinski space to a Boolean.
app_sigma :: Sigma -> Int -> Maybe Bool
app_sigma p k = case approximate p (prec RoundDown k) of
                   True  -> Just True -- lower approximation is True, the value is top
                   False -> case approximate p (prec RoundUp k) of
                              False -> Just False -- upper approximation is False, the value is bottom
                              True  -> Nothing


-- | The Show instance may cause divergence because 'force' could diverge. An alternative
-- implementation would give up after a while, and the user would have to use 'force' explicitly to
-- get the exact results (or divergence).

instance Show Sigma where
  show p = 
    let (b, k) = forceVerbose p in
    (show b) ++ " (needed precision " ++ (show k) ++ ")"

-- XXX - should the code above be in Sigma.hs, similar to Reals.hs? Ideally yes, but then Sigma and Space would need to refer to each other circularly.




-- | Partial booleans form a space in which @PTrue@ and @PFalse@ are uncomparable,
-- but both above ("more precise than") @PBottom@ (= "unknown truth value") and 
-- below @PTop@ (= "uncomputable; true and false simultaneously").
data PartialBool' = PTrue | PFalse | PBottom | PTop 
                  deriving Show
                          
-- | Literal equality of PartialBool'
(===) :: PartialBool' -> PartialBool' -> Bool
p === q = case (p,q) of
  (PTrue, PTrue) -> True
  (PFalse, PFalse) -> True
  (PTop, PTop) -> True
  (PBottom, PBottom) -> True
  (_, _) -> False

-- | Constructor for some PartialBool' objects
partial :: Bool -> PartialBool'
partial True = PTrue
partial False = PFalse
  

type PartialBool = Staged PartialBool'

-- | Force a partial boolean into a boolean, but only try with precision @maxPrecision@ or less. 
-- If it does not converge that fast, produce an error.
-- Negative maxPrecision indicates unlimited precision.
tryForceBool :: Int -> PartialBool -> Bool
tryForceBool maxPrecision p = loop 0
  where loop k = let lo = approximate p (prec RoundDown k) 
                     hi = approximate p (prec RoundUp k) 
                     err     = error $ "Internal error; upper and lower approximation disagree in an uncompatible way. Lower/upper approximation: "++(show lo)++"/"++(show hi)
                     top     = error $ "Meaningless expression; evaluates to PTop"
                     bot     = error $ "Uncomputable value; evaluates to PBottom"
                     timeout = error $ "Truth value did not converge at precision "++(show maxPrecision)++" or less. Last lower/upper approximation: "++(show lo)++"/"++(show hi)
                 in 
                  trace ("+++ RACUNAMO APROXIMACIJO ZA PartialBool Z NATANCNOSTJO DO "++(show k)++" +++") $ 
                  case (lo,hi) of
                   (PTrue, PTrue)     -> True
                   (PFalse, PFalse)   -> False
                   (PTop, PTop)       -> top
                   (PBottom, PBottom) -> bot
                   (PFalse, PTrue)    -> err
                   (PTrue, PFalse)    -> err                 
                   (PTop, _)          -> err
                   (_, PBottom)       -> err
                   (_, _)             -> -- approximations do not agree, but in a compatible way
                     if k >= maxPrecision && maxPrecision >= 0
                     then timeout
                     else loop (k+1)

-- | Force a partial boolean into a boolean. This may diverge as bottom cannot be
-- reliably detected.
forceBool :: PartialBool -> Bool
forceBool = tryForceBool (-1)

instance Show PartialBool where
  show p = show $ tryForceBool 10 p


-- | Disjunction for PartialBool space
por :: PartialBool -> PartialBool -> PartialBool
por = lift2 (\_s p q -> case (p,q) of
                (PTrue, _)  -> PTrue
                (_, PTrue)  -> PTrue
                (PFalse, q) -> q
                (q, PFalse) -> q
                (PBottom, PBottom) -> PBottom
                (PTop, PTop) -> PTop
                ( _, _) -> PTrue
            )

-- | Conjunction for PartialBool space
pand :: PartialBool -> PartialBool -> PartialBool
pand = lift2 (\_ p q -> case (p,q) of
                        (PFalse, _)  -> PFalse
                        (_, PFalse)  -> PFalse
                        (PTrue, q) -> q
                        (q, PTrue) -> q
                        (PBottom, PBottom) -> PBottom
                        (PTop, PTop) -> PTop
                        ( _, _) -> PFalse
                    )


-- | Negation for PartialBool space
pnot :: PartialBool -> PartialBool
pnot = lift1 (\s p -> case p of 
                 PTrue -> PFalse
                 PFalse -> PTrue
                 p -> p
             )
                 
       
       


-- | A space is Hausdorff if inequality, here called 'apart', is an open relation. 
-- XXX (OK) what is an "open relation"? An open relation between sets A, B corresponds to an open set in space AxB. How do we enforce openness of `apart`? By implementing it. (open set == verifiable property)
class Hausdorff t where
  apart :: t -> t -> Sigma 

-- | A space is Discrete if equality, here called 'equal', is an open relation.
class Discrete t where
  equal :: t -> t -> Sigma
  
-- | Suppose the type 's' represents a family of subspaces of 't'. The typical example is
-- that 't' is the type of reals and 's' is the type of closed intervals. Then the subspaces
-- represented by 's' are compact subspaces of 't' if the universal quantifier is a continuous
-- map from @t -> 'Sigma'@ to 'Sigma'. 
-- XXX (OK) - how do we enforce continuity of forall? It comes implicitly; computable functions == continuous functions
class Compact s t | s -> t where
  forall :: s -> (t -> Sigma) -> Sigma

-- | Suppose the type 's' represents a family of subspaces of 't'. The typical example is
-- that 't' is the type of reals and 's' is the type of closed intervals. Then the subspaces
-- represented by 's' are overt subspaces of 't' if the existential quantifier is a continuous
-- map from @t -> 'Sigma'@ to 'Sigma'.
class Overt s t | s -> t where
  exists :: s -> (t -> Sigma) -> Sigma

-- | The real numbers are strictly linearly ordered by open relation <, we define
-- a class that expresses that fact.
class LinearOrder t where
  less :: t -> t -> Sigma
  more :: t -> t -> Sigma

  -- default implemetnation of 'more' in terms of 'less'
  more x y = less y x

-- | Cartesian product of spaces.
data (ProductSpace a b) = ProductSpace (a, b)