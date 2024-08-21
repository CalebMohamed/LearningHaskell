whatIsAGoodExampleOfRecursion = "one example is a definition of the max function, which takes in a list of a type in the Ord typeclass and returns the maximum, this can easily considered in a recursive fashion if you where to compare the first element to the maximum of the rest and so on until you reach a singleton and the comparisons will then return back to the top:"

-- maximum' :: (Ord a) => [a] -> a
-- maximum' [] = error "maximum of empty list" 
-- maximum' [x] = x 
-- maximum' (x:xs)
--     | x > maxTail = x
--     | otherwise = maxTail
--     where maxTail = maximum' xs

-- more concise:
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list" 
maximum' [x] = x 
maximum' (x:xs) = max x (maximum' xs)