-- Recap: Functor's fmap :: (a->b) -> f a -> f b
-- Functor takes type constructors of kind * -> *

-- IO derives Functor, and '(->)' r dervies functor too!

{-
instance Functor IO where
    fmap f action = do
        result <- action
        return (f result)
-}
-- uses: mainly when you bind the value of an IO action to a name and then proceed to apply one function and then call it something else. It is just much cleaner and doesn't use a let

-- ok so now to explain what on earth '(->) r' is
-- r -> a === (->) r a :: * | as it is evaluated as a value
-- so (->) r :: * -> *
-- and (->) :: * -> * -> *

-- so the functions type constructor can be written as partially applied, it can't be written like (2+) as there is not that support for type constructors in the syntax

-- we find the definition in Control.Monad.Instances

{-
instance Functor ((->) r) where
    fmap f g = (\x -> f (g x))
-}

-- this is a bit weird but substituting it into the fmaap type declaration may help:

-- fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
-- === (a - b) -> (r -> a) -> (r -> b)

-- as i suspected, this is FUNCTION COMPOSITION

{-
instance Functor ((->) r) where
    fmap = (.)
-} -- so concise!

-- brief note on the polymorphic functions defined in typeclasses - you often ommit the type constraint of the function for brevity as it is already specified that the type is deriving the typeconstructor
-- fmap :: (Functor f) => (a -> b) -> f a -> f b

-- considering curried functions helps us to get a more wholisitc understanding of fmap as a function being mapped over a computation context:
-- in the same way: a -> b -> c === a -> (b -> c);
-- fmap :: (a -> b) -> (f a -> f b)
-- what this means is the fmap takes a function (a -> b) and returns a new function thats like the old but takes a functor as the parameter and returns a functor
-- this is called a lifting function

-- fmap (*2) :: (Num a, Functor f) => f a -> f a
-- fmap (replicate 3) :: (Functor f) => f a -> f [a]