-- in js, non-empty strings return true in if statements
-- lets implement this in Haskell - although its not very useful

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

-- note we had to put a in [] to make it concrete
instance YesNo [a] where
    yesno [] = False
    yesno _ = True

-- id is a function that takes a paarameter and returns the same thing
instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

{-
instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True -- because the author runs yellows
-} 

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult