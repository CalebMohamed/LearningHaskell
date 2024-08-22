-- this is an idea from maths that (f.g) (x) = f(g(x))
-- so the definition of . is as follows:
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- one use of . is when making functions to pass onto others, lambdas could be used but function comp is often more clear and concise.
-- say we want to make all numbers negative in a list:
makeNegative :: (Num a) => [a] -> [a]
makeNegative = map (\x -> negate (abs x))

makeNegative' :: (Num a) => [a] -> [a]
makeNegative' = map (negate . abs)

-- simplifying method: remove the last argument after a $ and function composition the lhs:
-- sum (replicate 5 (max 6.7 8.9))
-- = sum . replicate 5 . max 6.7 $ 8.9
-- in this way you are turning the lhs into one long specific function which takes the max's second argument as its parameter and then giving it to it at the end
-- so it does the same thing but with a lot less clutter.

-- the pointfree or pointless style is where you get rid of the reference to the list or argument in a function declaration in favour of currying for better readability:
sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (+) 0 xs
sum' = foldl (+) 0 -- this is in the pointless style

-- but we need to use function composition to put other function bodies in this clean style:

fn :: (RealFrac a, Integral b, Floating a) => a -> b
-- fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate . tan . cos . max 50 

-- WARNING!!! making long chains of function composition can be very hard to read, it is instead encouraged to use lets to break up the problem into more readable and maintainable chunks

-- a lovely rendition of our oddSquareSum from 'c' in light of this is this:
oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in sum belowLimit