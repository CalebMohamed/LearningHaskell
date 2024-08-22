-- the definition of the $ is the following:
($) :: (a -> b) -> a -> b
f $ x = f x

-- seems pretty useless, except the fact it has much lower precedence than normal function application. 
-- Consequently it is right-associative making it useful for readability and decreasing the amount of functions:
-- sum (map sqrt [1..130]) = sum $ map sqrt [1..130]

-- it isn't just handy to clean up code, it can also be used to map function application over a list of functions:
-- map ($ 3) [(4+), (10*), (^2), sqrt]