-- one of the simpler ways of defining a data type is using the 'data' keyword and each of the values are separated by pipe

data Bool' = False | True

-- you can also define values with multiple 'fields' - not technical term i don't think

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float 
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- so Circle is the value constructor and following it we can specify the types of the values it will contain and their number
-- these 'fields' are actually parameters for the functions called value constructors

-- :t Circle -> "Circle :: Float -> Float -> Float -> Shape"

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = width * height
--     where width = abs $ x2 - x1
--           height = abs $ y2 - y1

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = width * height
    where width = abs $ x2 - x1
          height = abs $ y2 - y1

-- we use Shape as the type not Circle in the type declaration, as Circle is a value constructor not a type, however we can pattern match against the constructor
-- [], False and 5 are all constructors yet we pattern match to them very often :O

-- in GHCI if we write out the constructor with its parameters we'll get an error because we can't show it
-- we simply need to derive the typeclass:
-- data Shape = ... | ... deriving (Show)
-- its been ammended above

-- as value constructors are functions we can partially apply them and then map them to a list!!!

myConcentricCircles :: [Float] -> [Shape]
-- myConcentricCircles = map (Circle 0 0)
myConcentricCircles = map (Circle (Point 0 0))

-- our data type could be clearer by using a point datatype that we create
-- this does make the pattern matching more cumbersome and the construction of the shapes from scratch but the code becomes much clearer

-- lets translate the shapes:

translate :: Shape -> Float -> Float -> Shape
translate (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
translate (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- we can then make some auxiliary functions to make shapes of some size at the origin:

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- this means we can more easily define the shapes with a translate and baseRect combination
-- translate (baseRect 40 100) -20 -50

-- to export datatypes you just need to add: type(const,ructor) to the list of exported functions, you can also just write type(..) to export all the value constructors for a given type

-- module Shapes
-- ( Point(..)
-- , Shape(..)
-- , surface
-- , translate
-- , baseCircle
-- , baseRect
-- ) where

-- you can also ommit the () from the type to not export the constructors in this way only the auxilliarly functions to generate the type.
-- consequently you can't pattern match against the constructor though