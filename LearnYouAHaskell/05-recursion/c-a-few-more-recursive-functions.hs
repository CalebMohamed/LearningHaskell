import Distribution.Compat.Lens (_1)
canYouShowMeSomeMoreRecursiveExamples = "Yes, there are many but ill just discuss the following: replicate - notably the use of a counting argument is used to restrict the number of recursions, also Num and Ord must be used as Num contains complex numbers which aren't ordered although most are. take - similarly counting with an argument but a neat use of the pattern fall through to check for a finished count OR an empty list. reverse - this has an interesting base case and cool idea which is that each recursion concats the head after the tail and then operates on tail to reverse it too. repeat - this has no base case as it produces an infinite list by design - here it helps that Haskell is lazy. zip - notable because of the dual symmetrical base case and pattern matching. elem - doesn't seem as interesting s the others."

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ []   = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []      = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs