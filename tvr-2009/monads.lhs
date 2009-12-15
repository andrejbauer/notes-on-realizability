HASKELL MONADS VIA EXAMPLES

This file explains what Haskell monads are. It is written in the "literal Haskell" format in which everything is a comment, except those lines that start with >. You can run the code from ghci by typing ":l monads.lhs" without quotes.

There are any number of introductions on monads written by Haskell bloggers around the internet. You may want to look at some of them for further insight. We shall introduce monads by examples.

Haskell is a purely functional language. In particular, it does not allow direct invocation of any operations that would allow us to detect the order of evaluation. This rules out mutable variables, exceptions, I/O, and many other useful programming concepts. All these can be put back into Haskell with a clever way of programming known as "monadic style".

Our first example is a very simple kind of exception which is sometimes known as "abort". It is an operation which aborts whatever is being computed, and cannot be intercepted. To have something like that in Haskell, we need to be explicit about values which may trigger abort. So we define a datatype Abortable a which means "either an ordinary value of type a, or a special value Aborted indicating that abort happened":

> data Abortable a = Value a | Aborted
>                    deriving (Eq, Show)

The constant Aborted signifies a value whose computation was aborted. The other possibility is a value of the form Value v, which signifies a successfully computed value v (abort did not happen). Haskell insists that we write Value v rather than just v. This way it can tell the difference between ordinary values and values that could have been aborted but were not.

An ordinary value v of type t may always be converted to an abortable value Value v of type Abortable t. The operation that does this is called "return" in Haskell and is the first half of a monad.

The second half of a monad is an operation >>= (called "bind") which combines an abortable value

    x :: Abortable a

and a function

    f :: a -> Abortable b

which expects an ordinary value and outputs an abortable one. This is written as

    x >>= f

What should x >>= f be? Well, if x is Aborted then x >>= f must also be Aborted (we want Aborted to act as an uncatchable exception). If x is of the form Value v then x >>= f should be f v. This brings us to the official monad definition:

> instance Monad Abortable where
>    return v        = Value v
>    Aborted >>= f   = Aborted
>    (Value v) >>= f = f v

In principle we can use return and >>= to compute with abortable values, but it is very cumbersome. For example,  suppose we have a division operation whose result is an abortable integer,

> divide :: Int -> Int -> Abortable Int
> divide x 0 = Aborted
> divide x y = return (x `div` y)

In order to compute the function which maps x and y to (x/y + y/x) we have to write

> f :: Int -> Int -> Abortable Int
> f x y = (divide x y) >>= (\u -> (divide y x) >>= (\v -> return (u + v)))

With good indentation it is possible to improve the code a bit:

> g :: Int -> Int -> Abortable Int
> g x y = (divide x y) >>= (\u ->
>         (divide y x) >>= (\v ->
>         return (u + v)))

This we can read as: feed the abortable value divide x y into u, feed the abortable value divide y x into v, then return the abortable value u + v. The operator >>= makes sure that the whole result is Aborted if either u or v is. Haskell has special notation which significantly improves the code:

> h :: Int -> Int -> Abortable Int
> h x y = do u <- divide x y
>            v <- divide y x
>            return (u + v)

The functions g and h are the exact same thing written in different notations.

The Abortable monad is already built into Haskell, except it is called the Maybe monad. Instead of Value v and Aborted it has Just v and Nothing. To give an example of its use, suppose we have an association list

> lst = [("apple", 3), ("orange", 10), ("banana", 2), ("stone", 6)]

and would like to find the value corresponding to "orange". We can do this by writing

> y = lookup "orange" lst -- y equals Just 10

The answer is Just 10 because lookup returns "Maybe" values (it may find something, or not). So if we lookup something that is not in the list we get Nothing:

> z = lookup "cow" lst -- z equals Nothing

The monad and do notation come in handy if we have a piece of code that does several lookups and we want it to fail as soon as one of the lookups fails, e.g.

> sum = do u <- lookup "banana" lst
>          v <- lookup "apple" lst
>          w <- lookup "cherry" lst
>          return (u + v + w)

The value of sum is Nothing because the third lookup fails. Had it succeeded, we would get Just i for some integer i. Here is one way of writing the same code without the do notation:

> sum' = case (lookup "banana" lst, lookup "apple" lst, lookup "cherry" lst) of
>           (Just u, Just v, Just w) -> Just (u + v + w)
>           (_, _, _) -> Nothing

You may judge for yourself which one is more readable.

> data Choosable a = Choices [a] deriving Show
> instance Monad Choosable where
>     return v = Choices [v]
>     (Choices vs) >>= f = Choices (foldl (\us v -> let Choices ws = f v in us ++ ws) [] vs)
