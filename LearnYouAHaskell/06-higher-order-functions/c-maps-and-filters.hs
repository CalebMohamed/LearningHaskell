whatDoesMapDo = "the map function applies a function to all elements of a list to produce a list of results: map (+3) [1,2,3] -> [4,5,6]"

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

whatDoesFilterDo = "the filter function takes a boolean function (predicate) and applies it to all elements appending them to a list of accepted elements which it returns: filter (>3) [1,2,3,4] -> [4]"

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

whyUseThese = "list comprehensions can easily do what map and filter do, but when applying multiple maps or filters list comprhensions can become very hard to read so its mainly for readability."

-- here's an example of a function that finds the largest number divisible under 100,000 that is divisible by 3829, so we take a range of the possible solutions and then filter them with a predicate which we define in the where and take the head - as that will be the largest one. Because Haskell is lazy it won't actually filter every item which is pretty neat.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- now I'm going to find the sum of all odd squares that are smaller than 10,000
-- first i find all squares map (^2) [1..]
-- then i am going to filter them by if they are odd
-- then i'm going to use the takeWhile function to take the values that are smaller thn 10,000
mySpecialSum :: (Integral a) => a
mySpecialSum = sum (takeWhile q (filter p (map (^2) [1..])))
    where p x = x `mod` 2 == 1
          q x = x < 10000

-- exploring collatz sequences
-- these sequences half when even and *3 +1 when odd
-- it is an unproven conjecture that these always return to 1
-- we want to find how many numbers between 1 and 100 have a chain length >15
collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
    | even n = n:collatzChain (div n 2)
    | odd n  = n:collatzChain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatzChain [1..100]))
    where isLong xs = length xs > 15

canIUseAMapToMakeAListOfPartiallyAppliedFunctions = "yes of course you can map * [0..] -> [(0*),(1*),(2*)..], which can then be accessed with !! and then applied to another number with a space: ((map * [0..]) !! 3) 5 -> 15 as the element at index 3 is (3*)."