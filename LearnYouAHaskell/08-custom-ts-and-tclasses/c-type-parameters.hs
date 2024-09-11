-- type constructors can take types as parameters and return new types.
-- a straight forward example is the Maybe type constructor

data Maybe a = Nothing | Just a

-- Maybe Int, Maybe Car, Maybe String all return types
-- but Maybe itself is partially applied and not a type itself

-- we actually used the type constructor [] before Maybe
-- [Int], [Char], [[String]] are all types but [] is not - crazy

-- more on Maybe a:
-- whats really cool about Nothing is that it is polymorphic, matching the Maybe a of the function it is passed into
-- so for any function requring maybe a we can just pass it Nothing
-- this is similar to how 5 can act like an Int or a Double
-- same as [a], as [1,2,3] ++ [] is valid as [a] takes on [Int]

-- type constructors are only really helpful when they act like a box where the type of the entered field doesn't matter
-- there's no need to generalise a type when it won't be used like that due to the ammount of inconvience that ensues

-- an example where it is quite useful, is the Map function that relates keys to values
-- data (Ord k) => Map k v = ...
-- the keys can be any type and the same with the values - they can be anything

-- its a strong convention in Haskell to not add typeclass constrainst in data declarations.
-- this is because you can add them to functions that require specific versions of the datatype but you can't leave them out of more general functions concerning said datatype if you leave it in the data declaration
-- it also requires you to still put the type constraint in all functions anyway

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vSum :: (Num t) => Vector t -> t
vSum (Vector i j k) = i+j+k

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` scalar = Vector (i*scalar) (j*scalar) (k*scalar)

dot :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `dot` (Vector l m n) = i*l + j*m + k*n

