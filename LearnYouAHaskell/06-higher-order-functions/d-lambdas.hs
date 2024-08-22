whatAreLambdasInHaskell = "lambdas are anonymous function declared with a \\ (just one) followed by its parameters and then a -> for its function body. We enclose them with parentheses so they don't extend to the right. You can pattern match but can't define multiple patterns and if a pattern falls through there will be a runtime error. Instead of a short function in a where binding we can use a lambda:"

-- numLongChains :: Int
-- numLongChains = length (filter (\xs -> length xs > 15) (map collatzChain [1..100]))

howNotToBeANoobWithLambdas = "noobs will often use lambdas in places where a partial application is absolutely fine: map (+3) [1,2,3] vs map (\\x -> x + 3) [1,2,3], but this is silly because the partial application is more readable."