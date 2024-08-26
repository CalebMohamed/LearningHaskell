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
-- consider (3x^2 + 5x + 9) + ()