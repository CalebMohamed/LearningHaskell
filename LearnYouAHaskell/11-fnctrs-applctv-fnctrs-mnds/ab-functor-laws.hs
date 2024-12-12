-- In order for things to be functors they should satisy some laws, as all good mathematical objects do
-- They should reliably behave as things that can be mapped over, calling fmap on a functor should just map the function over the functor nothing more.
-- There are 2, but Haskell doesnt enforce them so we have to

-- 1. if we map the id function over a functor the functor that we get back should be the same as the original functor
-- fmap id = id

-- 2. composing two functions and then mapping the resulting function over a functor should be the same as mapping them one by one in a pipeline fashion
-- fmap (f . g) = fmap f . fmap g

-- an example that derived Functor but isn't a true functor

{-
-- short for Maybe with Counter
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)
-}

-- this doesn't obey either laws, see counterexamples
-- 1. identity law
-- fmap id (CJust 0 'a') -> CJust 1 (id 'a') -> CJust 1 'a'
-- CJust 0 'a' /= CJust 1 'a' => CMaybe is not a functor
-- 2. composition law
-- fmap (f.g) (CJust 0 'a') -> CJust 1 (f.g 'a')
-- fmap f $ fmap g (CJust 0 'a') -> fmap f (CJust 1 (g 'a')) -> CJust 2 (f(g 'a')) === CJust 2 (f.g 'a')
-- CJust 1 (f.g 'a') /= CJust 2 (f.g 'a') => CMaybe is not a functor

-- this is tricky because the way in which you map multiple functions over this fake functor can have different results, making this very prone to faults when you or others use this datatype later. 
-- Functor derivatives are expected to be true functors so that these behaviours are more uniform and errors are avoided

-- A note explaining contexts a bit more:
-- Just 3 outputs value 3 in the context that it might be an output or nothing at all
-- [1,2,3] outputs 3 values in the context that there may be multiple or no values
-- (+3) outputs a value in the context that it is dependent on the parameter it is given
-- functors are these contexts which can be mapped over by a function so that the function is applied to the result of the computation without error
-- in this way functors are outputters and mapping over them is a kind of transformation or modifier to their output value