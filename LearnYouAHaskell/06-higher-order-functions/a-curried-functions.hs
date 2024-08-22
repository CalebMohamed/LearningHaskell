whatAreHigherOrderFunctions = "functions that take other functions as parameters or return functions as return values. Think back to lambda calculus, these are indespensible tools to make complex functionality."
whatIsCurrying = "every function actually takes only one parameter (like lambda calculus), and so all functions with multiple parameters are curried functions - that is it applies the first input and returns a more specific function with the first input subsituted in which is then applied to the second input and so on. This is why we need spaces between inputs because the space indicates a function application, and also why we need arrows between the types of the function definition as thats showing a return: a -> a -> a == a -> (a -> a)."
reframingTheFunctionEquals = "if we begin the consider the function body = as a queue to replace the LHS with the RHS, then you can see how we can write more efficient code by returning functions, see the example:"

-- compareWithHundred :: (Num a, Ord a) => a -> Ordering -- this is just the GT, LT, EQ type
-- compareWithHundred x = compare 100 x
-- here you can see how we could 'cancel out' the x on either side as we are only really replacing the compareWithHundred with 'compare 100' which then evaluates with the x following, we don't need to replace compareWithHundred x with compare 100 x, you can just leave the x alone.
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

canIPartiallyApplyInfixFunctions = "yes, but you need to surround them with parentheses. The function would get replaced with the infix function in parentheses and then it will behave like a postfix function:"

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

whatIsTheExceptionWithUsingInfixFunctionLikeThat = "the - operator plays a little strangely. (-4) x could be assumed to subtract 4 from x, however (-4) is defined as the negative number, so we need to use the infix function 'subtract': (subtract 4) x is what we are looking for."
partiallyApplyingFunctionsAndGHCI = "interesting if you partially apply values to a function - but not all of them GHCI throws an error. This is because a function was returned and functions are not in the Show typeclass so GHCI can't display the result."