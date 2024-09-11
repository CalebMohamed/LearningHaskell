-- the data list module is all about lists
-- it provides many useful functions some of which Prelude steals and imports itself: like map and filter
-- consequently they don't conflict and you don't need to qualify this module.

-- some cool functions:

-- intersperse - takes an element and a list and puts the element in between each pair of elements in the list:
-- intersperse '.' "MONKEY" -> "M.O.N.K.E.Y"
-- intercalate - inserts a list between each lists in a list of lists and flattens the result into one
-- intercalate " " ["hey", "there", "folks"] -> "hey there folks"
-- transpose - transposes a list of lists by combining the first elements of the sub lists into the a new sublist and 2nd elements into a new 2nd sublist and so on flipping them as if they were a matrix.
-- transpose [[1,2,3], [4,5,6], [7,8,9]] -> [[1,4,7],[2,5,8],[3,6,9]]

-- transpose has a lovely application for summing the coefficients of polynomials or other distinct terms in series:
-- consider (3x^2 + 5x + 9) + (10x^3 + 9) + (8x^3+5x^2+x-1)
-- represented as lists (4d vectors):
-- [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]
-- transpose it to place all the corresponding coeficients in their own lists:
-- transpose [...]
-- then map sum onto the list of lists so all of the lists are collapsed into their sums
-- map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]] -> [18, 8, 6, 17]

-- strict folds: Data.List implements strict versions (as opposed to lazy versions) of foldl, foldl1, foldr,... but with apostrophes at the end foldl' etc. 
-- In their lazy implementation the accumulator promises to compute its value when the result is asked for (this is called a thunk), but these thunks fill up the stack and can lead to stack overflows when operating on large lists. 
-- In their strict form the accumulator is actually updated and so the thunks don't fill up the stack.

-- concact - flattens one layer of nested lists.

-- concatMap - maps a function to each list and then concatenates the results with concat.

-- and - returns true if all elements of a list are true

-- or - returns true if one element from a list is true

-- any and all - these are like 'and' and 'or' but take a predicate and list applying the predicate to all elements and then the 'and' or 'or'.
-- any (==4) [2,3,5,6,1,4] -> True
-- all (>4) [6,9,10] -> True

-- iterate - applies a function repeatedly to a starting result creating an infinite list of results, this can then be 'take'en from to get a sequence of repeated function calls on the same starting input:
-- take 5 $ iterate (*2) 1 -> [1,2,4,8,16]

-- splitAt - splits a list before the given index returing a pair of two lists
-- splitAt 3 "heyman" -> ("hey", "man")
-- splitAt 100 "heyman" -> ("heyman", "")

-- takeWhile - takes elements while the element satisfies the predicate and then cuts off
-- takeWhile (>3) [6,5,4,2,10,2,3,5] -> [6,5,4]

-- determine the sum of all third powers under 10,000, we can't filter an infinite list so we need to takeWhile
sumThirdPowsBelowBig :: Int
sumThirdPowsBelowBig = sum $ takeWhile (<10000) $ map (^3) [1..]

-- dropWhile - is the same as takeWhile but dropping instead

-- span - this is list takeWhile but returns (take-part, and-the-rest)

-- break - like span but first breaks when the predicate is true so that the first true evaluating element starts the second list of the pair.

-- sort - this simply sorts a list of Ord elements into asc order

-- group - this takes a list and groups adjacent repeat elements into sublists
-- group [1,1,1,2,2,3,1] -> [[1,1,1], [2,2], [3], [1]]

