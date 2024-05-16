main = do
    putStrLn "Hello, world!"
    putStrLn ("Please look at the following even numbers: " ++ show (filter even [10.. 20]))