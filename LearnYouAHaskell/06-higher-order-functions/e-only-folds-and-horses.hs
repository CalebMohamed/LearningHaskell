whatIsAFold = "often in the examples of recursion we use an edge case, and an x:xs pattern to recursively perform a function on each element in a list. Because this is so common a class of functions where created to perfom a similar thing where they reduce a list to a single value. foldl - left fold, folds a list down from the left - it takes as parameters a binary function, a starting value and a list to fold. the binary function is called with the current value and the new head of the list before the head is removed:"

-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (\acc x -> acc + x) 0
-- more concise
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- a cool case with a definition of the elem function
elem' :: (Eq a) => a -> [a] -> Bool
elem' y = foldl (\ acc x -> if x == y then True else acc) False
-- this will start off false and turn true and never false again if it find the search term

-- right fold is very similar, but takes the acc as the second argument in the binary function
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
-- we could have used a foldl but then we would have to use ++ which is more expensive as the list is on the left side of the accumulation

-- there are different types of folds:
-- foldl left to right fold
-- foldr right to left fold
-- foldl1 left to right assuming 1st element as starting
-- foldr1 right to left assuming last element as starting
-- consequently foldl1 and foldr1 can only be applied to lists of length 1 or more

-- a bunch of standard library functions implemented with folds:

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- scanl, scanr, scanl1, scanr1 are like folds but they store their accumulator values as a list

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- takeWhile instead of filter as the list is infinite