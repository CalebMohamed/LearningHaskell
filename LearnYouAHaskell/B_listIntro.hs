whatAreListsInHaskell = "lists are a homogenous data structure which can be defined with let in ghci"
howDoIDefineThem = "you can use comma separated values in [] like in C"
howDoIConcatenate = "just use ++, tip: haskell walks through all elements of the left list so don't do [1,2,3....1000] ++ [0], instead do [0] ++ [1,2,3....1000]"
whatIsTheConsOperator = "this is the : that allows you to instantly put something at the start of a list, A : SMALL CAT, [1,2,3] = 1:2:3:[] under the hood"
howDoIAccessElements = "use the !! operator, Cat !! 2 = t"
whatDoesComparingListsDo = "[3,1,2] > [1,3,2] = True, because it compares them lexicographical (pairs of elements with each other then the next if they are equal)"
listOperators = "head, tail, last, init, length, null, inverse, take, drop, maximum, minimum, sum, product, elem, " 
listOperatorsThatNeedADescription = "tail (everything but head), init (inverse of tail), null (checks if the list is empty to catch the errors that arise from running the other operations on an empty list), take (returns first n elements), inverse of take (returning all but first n), elem (takes a thing and a list, and returns true if the thing is in the list)"