import Control.Arrow (ArrowChoice(right))
-- you can do some really cool stuff if you make a datatype reference itself
-- infact list is defined just like this

-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- this is cool because the list definition is empty or contains the current value & a List of the same type
-- which itself can be empty or contain the next element and the next part of the list
-- this is the same as a:(b:[])
-- 4 `Cons` (5 `Cons` Empty)

-- we can make functions automatically infix if they are only special characters:
-- when defining one like this we give it a binding strength (fixity), higher fixity operations will execute first
infixr 5 :+:
data List a = Empty | a :+: (List a) deriving (Show, Read, Eq, Ord)

mySpecialList :: List Int
mySpecialList = 3 :+: 4 :+: 5 :+: Empty

infixr 5 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :+: xs) .++ ys = x :+: (xs .++ ys)

myMoreSpecialList :: List Int
myMoreSpecialList = (3 :+: 4 :+: 5 :+: Empty) .++ (6 :+: 7 :+: Empty)

-- whats cool here is that the pattern mathc of x :+: xs works
-- this is because pattern matching is actually about the constructors really

-- lets make a tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- as this is functional to build the tree by inserting a node at a time, we have to return a new tree after navigating to the correct place

genSingletonTree :: a -> Tree a
genSingletonTree x = Node x EmptyTree EmptyTree

-- whats notable here is that we only introduced the Ord restriction in the insert function as the singleton could be a 'tree' with unordered data as there is only one data point
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = genSingletonTree x
treeInsert x (Node a left right)
    | x == a = Node a left right -- return same
    | x < a = Node a (treeInsert x left) right -- try to insert left
    | x > a = Node a left (treeInsert x right) -- try to insert right

-- now lets implement a binary tree search
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem searchTerm EmptyTree = False
treeElem searchTerm (Node currentValue left right)
    | searchTerm == currentValue = True
    | searchTerm < currentValue = treeElem searchTerm left
    | searchTerm > currentValue = treeElem searchTerm right

genTree :: (Ord a) => [a] -> Tree a
genTree = foldr treeInsert EmptyTree