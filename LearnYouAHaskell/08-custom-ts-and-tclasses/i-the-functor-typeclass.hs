-- Functor typeclass from category theory
-- This typeclass is for things that can be mapped over.

-- lists are an instance of the functor type class as they can be mapped over

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}

-- its notable that f is a type constructor because in the type definition of fmap it combines with a and b to form the types

-- so the fmap function takes a function that takes type a and returns type b
-- then it also takes a functor instance of type a
-- and finally returns a functor instance of type b

-- this is very similar to the map function:
-- map :: (a -> b) -> [a] -> [b]
-- and thats because [] is the functor instance and:

{-
instance Functor [] where
    fmap = map
-}
-- i think the actual implementation of fmap is not shown in the tutorial but it behaves like map (it is map)
-- in that it applies a function to every element of the list and then collates them in a list of the second type

-- any types constructors that act like boxes for other types can be functors:

{-
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
-}

-- we can get a picture of how fmap might be defined when we define it for our recursive tree class:

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

-- Either can also be a functor but as functors only take one box to convert to another type, the right and left can't both have a function applied to them as they may not have the same type
-- so we only apply the map the right part
-- this allows us to write the type constructor partially applies so that it fits the same type signature as f

{-
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x -- left alone
-}

-- you can also make Data.Map Maps functors with a similar method mapping v -> v' in Map k v

-- my guess -- incomplete
{-
instance Functor (Map k) where
    fmap f (Map k v) = Map k (f v)
-}