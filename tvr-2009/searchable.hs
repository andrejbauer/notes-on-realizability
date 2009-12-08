module Searchable where

data Searchable a = Finder ((a -> Bool) -> a)
find :: Searchable a -> (a -> Bool) -> a
find (Finder epsilon) p = epsilon p

-- auxiliary function search
search :: Searchable a -> (a -> Bool) -> Maybe a
search s p =
    let x = find s p
    in if p x then Just x else Nothing

-- quantifiers

exists s p = p (find s p)

forall s p = not (exists s (not . p))

-- some searchable spaces

-- singleton
singleton x = Finder (\p -> x)

-- doubleton
doubleton x y = Finder (\p -> if p x then x else y)

-- finite non-empty sets
finite_set :: [a] -> Searchable a

finite_set lst = Finder (\p ->
    let loop []     = undefined
        loop [x]    = x
        loop (x:xs) = if p x then x else loop xs
    in loop lst)

-- the sum of two searchable sets a and b is searchable
sum a b = Finder (\p -> let x = Left (find a (p . Left))
                        in if p x then x else Right (find b (p. Right)))

-- a union of a searchable family of searchable spaces as
bigUnion :: Searchable (Searchable a) -> Searchable a
bigUnion as = Finder (\p -> find (find as (\s -> exists s p)) p)

-- a union of two sets is a special case
union a b = bigUnion (doubleton a b)

-- the image of a searchable set a under a map f : a --> b
image f a = Finder (\p -> f (find a (p . f)))
                              
-- monad structure
instance Monad Searchable where
    return = singleton
    xs >>= f = bigUnion (image f xs)

-- product of a and b
a `times` b = Finder (\p -> do x <- a
                               y <- b
                               return (x, y))

-- a product of a list of spaces
prod [] = return []
prod (a:as) = do x <- a
                 xs <- prod as
                 return (x:xs)

-- the Cantor space
two = doubleton False True
cantor = prod (repeat two)

-- we may test equality of functions
equal a f g = forall a (\x -> f x == g x)