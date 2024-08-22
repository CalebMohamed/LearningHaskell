exploringFunctionsAsParameters = "consider the applyTwice function which takes a function and value and applies the function twice. As you'll see in the type declaration we have to use parentheses to show we are taking a function as a parameter:"

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

canYouShowMeACoolUseOfAllOfThePreviousChapters = "yes check out the following definition of the zipWith function which takes a function and two lists and applies the function to each list pair combining them into one list and stopping when one list ends:"

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- as you can see we are using recursion, and pattern matching, as well as an interesting type declaration.

-- check out this funky function, it makes a function take its arguments in reverse order the brackets are ommitted on the right because -> is right associative
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y