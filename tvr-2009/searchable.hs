module Searchable where

import Data.List (find)

data Searchable a = Finder ((a -> Bool) -> a)
search :: Searchable a -> (a -> Bool) -> a
search (Finder epsilon) p = epsilon p

-- quantifiers

exists (Finder epsilon) p = p (epsilon p)
forall s p = not (exists s (not . p))

-- some searchable spaces
finite_set :: [a] -> Searchable a

finite_set lst = Finder (\p -> case find p lst of
                                 Nothing -> head lst
                                 Just x -> x)
