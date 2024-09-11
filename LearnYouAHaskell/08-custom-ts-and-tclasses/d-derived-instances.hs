import Data.Time (DayOfWeek(Saturday))
-- Haskell can automatically make our type an instance the following typeclasses when we use the deriving keyword: Eq, Ord, Enum, Bounded, Show, Read
-- Haskell will simply accept a type as Eq if all of its values in the constructor are themselves instances of the Eq typeclass.

-- show and read act in the same way, automatically being associated with the deriving keyword if each value is already an instance.
-- remember for the read we need to specify the type we want the string to be cast to, but we can ommit this if it is implied by its use such as equating it to the right type

-- you can also read parameterised types but you have to fill in the type e.g.
-- read "Just 't'" :: Maybe Char

-- the Ord class can be automatically derived with deriving if you have multiple values split by pipes in which case it will read from lowest to highest from left to right
-- for the Maybe a datatype Nothing is before Maybe, but in the case it is two Nothings compared or two Maybes then the values inside are compared if they too are Ord.

-- for the Enum typeclass you just need the constructors to take no values and then the pipes define precedecessors and successors
-- similarly the Bounded typeclass also applies to the pipes

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

