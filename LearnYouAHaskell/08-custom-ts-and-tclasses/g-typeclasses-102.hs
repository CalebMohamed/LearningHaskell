-- as we know typeclasses define a bunch of functions that much be able to be executed on its instances

{-
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y) 
-}

-- the a after Eq can be anything but a is convenient
-- swapping a with equatable may be easier

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False -- this pattern catches all other combinations

-- now it makes sense why we defined == and =/ recursively in the Eq' definition, because it means that we only have to override one for the other to be true
-- without those two lines we'd have to write down all the cases where it returns false which is a lot of combinations

instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow Light"
    show Green = "Green Light"

-- you can add class constraints to class definitions to make a kind of subclass
-- Num is defined as:
-- class (Eq a) => Num a where ...

-- in the case where we are defining the instances from a type constructor we can't just write the type constructor in place of a, it isn't concrete!

{-
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}

-- as you can see here we get rid of the Justs and only compare the contents for the equality
-- so the contents must themselves be instances of Eq
-- hence the class constraint

-- in haskell because its for maths nerds, variable names are preferably single letters with the types and functions providing the readability

-- TO SUMMARISE, class constraints in class declarations are used to make a typeclass a subclass of other classes, and class constraints in the instance declarations express requirements for the contents of some type

-- do :info {yourTypeClass} in ghci to see the instances of a typeclass and the functions
-- :info works for types and type constructors too