whatIsASetComprehension = "this is a way of defining a more specific set from another: S = { 2 ⋅ x | x ∈ N, x ≤ 10 }, the LHS is an output function, and the RHS is the set you are parsing through that output function"
whatIsAListComprehension = "this is the Haskell equivalent of a set comprehension, [ f(x) | x <- [list] ], e.g. [ x*2 | x <- [1..10] ]"
whatIsAPredicate = "you can add conditions to the input x's (adding a predicate is called filtering) as a 3rd expression: [ f(x) | x <- [list], f(x) <= b ] or another condition just like the set comprehension, you can add multiple predicates"
canIDrawFromMultipleLists = "yes, [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]"
howDoIDealWithListsOfLists = "you can nest list comprehensions, pretty cool"

length' xs = sum [1 | _ <- xs]
removeUppercase st = [ c | c <- st, c `elem` ['a'..'z'] ]

-- doesn't work yet
-- fizzBuzz xs = [ fbReplace x | x <- xs]

-- fbReplace :: String -> String
-- fbReplace x
--     | x `mod` 3 = if x `mod` 4 then "fizzbuzz" else "fizz"
--     | x `mod` 5 = "buzz" 
--     | otherwise = 'x'