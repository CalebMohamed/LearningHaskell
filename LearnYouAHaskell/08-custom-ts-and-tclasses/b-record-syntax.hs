-- using the last method of defining types it gets really cumbersome to reference an individual parameter of a type constructor you have to pattern match and then have a bunch of _s
-- look:

data Man = Man String String Int Float String String deriving (Show)

firstName' :: Man -> String
firstName' (Man firstName _ _ _ _ _) = firstName

-- and then you have to do that for every parameter

-- fortunately you can write types with record syntax:

data NewMan = NewMan { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- as you may suspect from the common 'firstName', they are both functions
-- haskell automatically makes functions for the paramters of the constructor when written in record form
-- it also makes the show form a lot more readable as it labels each of the fields with their names
-- you can also construct with a lot more syntactical clarity and then ordering of fields doesn't matter:

data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

-- ghci> Car {company="Ford",model="Mustang",year=1967}

-- in some data types like Vector, the normal format is fine but for others to distinguish and arbritray ordering of data its good to name them in the record format