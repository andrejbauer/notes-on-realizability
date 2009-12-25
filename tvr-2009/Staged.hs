{- | A staged computation is a sequence of approximations which represents its limit.
  Specifically, we think of modeling a topological space as a subspace of a continuous
  domain with a countable base, for theoretical background see the paper
  http://dx.doi.org/10.1016/j.apal.2008.09.025 (also available at http://math.andrej.com/)
    
  Suppose we want to model a continuous domain D with a base B. We first define a datatype @b@
  which represents the elements of B. It is natural to represent D with the datatype @Int -> b@,
  however, since in practice we also worry about rounding, we define a slightly more complicated
  datatype 'Stage' which also carries the rounding mode, and use @Stage -> b@ instead. The rounding
  mode is /not/ that of floating point arithmetic. Rather, it tells us whether the exact results
  should be approximated from below or above in the domain ordering. (Typically, computations based
  on domain-theoretic models always approximate from below, but there are uses for over-approximations
  as well, for example when we estimate the truth value of an existential quantifier.)

  It is cumbersome to work with the datatype @Stage -> b@ explicitly because we need to manually pass
  around the @Stage@ parameter. Haskell comes in handy here, as we define a monad which is very much
  like the @Reader@ monad of Haskell.
-}

module Staged where

-- | The rounding mode tells us whether we should under- or over-approximate the exact result
data RoundingMode = RoundUp | RoundDown
                  deriving (Eq, Show)

-- | A stage of computation tells us how hard we should try to compute the result. The 'stage' component
-- is a measure of precisions. As it goes to infinity, the approximation should converge to the exact
-- value (in the sense of Scott topology on the underlying domain model).
data Stage = Stage { stage :: Int, rounding :: RoundingMode }
             deriving Show

-- | 'anti' reverses the rounding mode
anti :: Stage -> Stage
anti s = Stage {stage = stage s, rounding = case rounding s of { RoundUp -> RoundDown ; RoundDown -> RoundUp}}

-- | 'down' sets the rounding mode to 'RoundDown'
down :: Stage -> Stage
down s = Stage {stage = stage s, rounding = RoundDown}

-- | 'up' sets the rounding mode to 'RoundUp'
up :: Stage -> Stage
up s   = Stage {stage = stage s, rounding = RoundUp}

-- | @prec k@ returns the @k@-th stage with rounding mode 'RoundDown'
prec :: Int -> Stage
prec k = Stage {stage = k, rounding = RoundDown}

-- | If @t@ represents the elements of a base for a domain, @Staged t@ represents the elements of
-- the completion of the base.
newtype Staged t = Staged { approx :: Stage -> t }

-- | The default stage to be used when outputting approximate results
default_stage :: Stage
default_stage = prec 10

-- | As a convenience we define a 'Show' instance for the elements of the domain. This is not an ideal
-- solution because it has a hard-coded 'default_stage' parameter which cannot be interactively changed
-- (what would be a better solution?)
instance Show t => Show (Staged t) where
  show x = show (approx x default_stage)

-- | The monad structure of 'Staged' is the same as that of the @Reader@ monad.
instance Monad Staged where
  return x = Staged $ \s -> x
  x >>= f  = Staged $ \s -> approx (f $ approx x s) s

-- | The functor structure of 'Staged' is the same as that of the @Reader@ monad.
instance Functor Staged where
  fmap f x = Staged $ \s -> f (approx x s)
    
-- | In a monadic computation, return the current 'stage' 
get_stage :: Staged Int
get_stage = Staged stage

-- | In a monadic computation, return the current 'rounding' mode
get_rounding :: Staged RoundingMode
get_rounding = Staged rounding
