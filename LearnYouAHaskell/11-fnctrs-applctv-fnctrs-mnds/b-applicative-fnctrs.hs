-- Applicative functors are 'beefed up' functors represented in Haskell by the Applicative typeclass, found in the Control.Applicative module.

-- in haskell we know we can curry functions and that a partially applied function is also a type so:
-- fmap (*) (Just 3) -> Just ((*) 3)
-- Just ((*) 3) :: Maybe (Num -> Num)

-- to use these we can just map a function which takes the function in the functor and applies it to a value:
-- let a = fmap (*) [1..4]
-- fmap (\f -> f 2) a -> [2,4,6,8]

-- what do we do if we want to take the function out of one functor and map it over another?
-- the Applicative typeclass defines with just their type declarations
{-
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

-- 1st line - all Applicative Functors are Functors so they can be fmap'd over
-- 2nd line - this quite clearly takes any value and wraps it in the compuational context of the Applicative Functor f. One way of thinking of it is that it takes a value and puts it in a default context - a minimal context that still yields that value
-- 3rd line - this is like fmap but extracts the function from a functor and then returns a function which takes a functor and returns a functor.
-- The extraction can be better thought of run then extract

{-
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something
-}
-- this is quite self explanatory, doing nothing as there is no function in a nothing
-- and pattern matching against that function in a just

-- Applicative functors allow you to operate on serveral functors with a single function because you can curry the function and apply it again
-- meaning that you can call pure on a function that takes multiple parameters to then feed it functors of the parameters:
-- pure (+) <*> Just 3 <*> Just 5 -> Just (((+) 3) 5) -> Just 8
-- pure (+) <*> Just 3 <*> Nothing -> Nothing

-- a cool oberservation is one of the applicative laws:
-- in the context of the applicative functor with constructor Oh
-- pure f <*> x -> Oh (f x)
-- fmap f x -> Oh (f x)
-- therefore: pure f <*> x === fmap f x

-- Control.Applicative exports a function called <$> which is just fmap as an infix operator
{-
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
-}
-- this allows us to use some nice aesthetic syntax for applying multiple parameters to a function in the context of an applicative functor:
-- f <$> x <*> y <*> z
-- SHEESH
-- (++) <$> Just "john tra" <*> Just "volta" -> Just "john travolta"

{-
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
-}
-- [(*0),(+100),(^2)] <*> [1,2,3]
-- [0,0,0,101,102,103,1,4,9]
-- we see the number of elements is [n] * [m]

-- applicative style can sometimes be a more readable way of writing list comprehensions

-- [x*y | x <- xs, y <- ys] === (*) <$> xs <*> ys

{-
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
-} -- i like this! you can see so clearly how a and b are unwrapped and then wrapped with the return

-- with IO it makes more sense to say run and extract or sequencing the IO actions into one

{-
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
-}
-- the pure function makes a function that takes any parameter and returns the result - its not a value itself
-- this is a bit of a mind bending applicative functor, where you can apply a function to the result of another function
-- it sounds a lot like composition until you realise you can use a partially applied function to sequence multiple functions into another function pipelining the results of multiple functions into another
-- according to the tutorial its usually only used for code golf
-- (+) <$> (+3) <*> (*100) -> (\x -> (3 + x) + (100 * x))
-- (+) <$> (+3) <*> (*100) $ 5 -> 508

-- we came across an interpretation of applicative lists where each function in a list is applied to each element
-- another interpretation is the zip-like applying each function in the function list to the corresponding element in the paramters list
-- we use a new instance as there can't be one type can't have two instances in a typeclass
-- ZipList a is a type with a field containing a list that is an instance of Applicative and describes such behaviour

{-
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
-}
-- here the pure function makes an infinite ZipList of the value as it is the simplest computational context that will work with any list of fs
-- the <*> function is quite self explanatory zipping the functions and paramter lists together by applying the functions

-- the pure function also makes sense of the id law:
-- pure id <*> (ZipList [1,2,3]) -> ZipList [1,2,3]
-- but if pure didn't repeat id:
-- pure id <*> (ZipList [1,2,3]) -> ZipList [1]
-- so the id law would be broken

-- Function in the tutorial right now:
-- ===================================
-- ! (,,) = \x y z -> (x,y,z)
-- ! (,) = \x y -> (x,y)
-- ! getZipList - takes a ZipList and returns its list
-- ! zipWith, zipWith3,..., zipWith7 - std library implements all of these with the numbers corresponding to the number of paramters the zipping function takes and the number of lists zipped together

-- ! liftA2 - this function takes a function and then two values in a specific applicative context, then applies the function to them in the same way we have been
{- 
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c 
liftA2 f a b = f <$> a <*> b
-}

-- lets make a demo function that takes a list of applicatives and returns one with a list inside it
import Control.Applicative ( Applicative(liftA2) )
sequenceA' :: (Applicative f) => [f a] -> f [a]
-- sequenceA' [] = pure []
-- sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs
sequenceA' = foldr (liftA2 (:)) (pure [])

