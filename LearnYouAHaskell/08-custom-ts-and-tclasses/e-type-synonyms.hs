-- type synonmys are where two types are equivalent like string and [Char]
-- They don't have any functionality but help with code readibility

-- type String = [Char]

-- the type keyword may be misleading because we use data to make something new but type to name the types more convinient things

-- we know that a map is just a list of key value pairs where the keys are instances of the Ord typeclass
-- so we could define a map as [(String,String)] for a specific case

phoneBook :: [(String,String)]
phoneBook =
    [("amelia","555-2938")  
    ,("freya","452-2928")  
    ,("isabella","493-2928")  
    ,("neil","205-2928")  
    ,("roald","939-8282")  
    ,("tenzing","853-2492")  
    ]

-- we can use type synonyms to make the type declaration more meaningful
-- type PhoneBook = [(String,String)]
-- consequently our phoneBook could be defined as phoneBook :: PhoneBook

-- we can go further!
-- this is very common in Haskell to convey more info about what strings in their functions should be used for
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

-- consequently type declarations become much more informative

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnum pbook = (name,pnum) `elem` pbook

-- without type synonyms: String -> String -> [(String,String)] -> Bool
-- as you can see these are good as a form of documentation and way to shorten lengthy type names

-- type synonyms can be parameterized too
type AssocList k v = [(k,v)]
-- we can also use them as partially applied type parameters:
type IntAssoc v = AssocList Int v

-- there is a really helpful type that takes two data types:
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
-- it can be used to return results of two different types quite easily
-- then the Left and Right constructors can be used to pattern match for the data in each case with the right body to handle the different type
-- an example might be a locker code lookup that returns a string saying the locker is already in use (or doesn't exist) or the integer code if it isnt, here you are returning Either String Int, and then the Left Right patterns can be used to handle the result.