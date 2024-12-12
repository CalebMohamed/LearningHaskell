-- WE ARE FINALLY GOING TO WRITE OUR FIRST 'REAL PROGRAM'


-- main :: IO () by convention we don't usually specify a type declaration for main as the () can be filled with different things depending of the contents

-- main = putStrLn "Hello, World!"

-- so the type of putStrLn as is indicated by the type declaration
-- putStrLn :: String -> IO ()

-- This IO () type is read as and Input/ Output with a result type of ().
-- This type is for actions with a side-effect, either reading from the input or printing to the screen, and will contain some kind of return value.
-- the tuple is empty because, there isn't a meaningful return value for printing to the screen

-- IO actions are only executed in main
-- if we want to bundle together multiple IO actions we can use main = do and then indent the actions.

{-
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey" ++ name ++ ", you rock!")
-}

-- name <- getLine binds the value of getLine into the variable name
-- getLine :: IO String
-- getLine has a string as the result type which makes sense as it is reading a string from the user's input

-- although the getLine is impure and so must be isolated from the pure code, when we bind it to name its just a normal string
-- so we can then call a pure funtion with name as a parameter and purity will still hold for that function
-- in that way the binding operator <- can allow us to bridge between impure and pure parts of the program
-- name = getLine, simply gives getLine the name 'name' like with type synonyms
-- we have to use the binding operator <-

-- whats cool is that this separation is enforced by the type system
-- nameTag = "Hello, my name is " ++ getLine doesnt work
-- this is because (++) :: String -> String -> String
-- yet getLine :: IO String, so the type enforces purity and separation of pure and impure functions

-- interestingly as any IO action has a return we could have done

{-
main = do
    foo <- putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey" ++ name ++ ", you rock!")
-}

-- where () - the singleton - is bound to foo
-- interestingly YOU CANNOT BIND THE LAST ACTION TO A NAME in the do block
-- this will be explained in Monads!!!

-- you can also do IO actions in GHCI and press enter

-- now lets are quite helpful in do blocks
-- as like in list comprehensions you don't have to use the in part, their scope if just whatever follows:

{-
import Data.Char  
  
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
-}

-- lets analyse a program that will continuosly read a line and print out it with the words themselves reversed until an empty line is input
-- interestingly we are using recursion to continue indefinitely until a condition is met (null line)
-- also we use 'return ()' to return the do blocks value
-- this is because if have to result in the output type of the function and main = do must end in an IO action
-- so the if statement needs to do the IO action return () - a nice action which simply takes the parameter as its payload and does nothing :D
-- or it needs to call itself again

main = do
    line <- getLine
    if null line
        then return ()
     -- then do return () -- alternatively
        else do
            putStrLn $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- just a little more on the return () 
-- it really is nothing like imperative returns
-- its just a useful, useless IO action:

{-
main = do
    return ()
    return "HAHA"
    line <- getLine
    return "BLAH"
    return 4
    putStrLn line
-} -- will still print the line out going all the way to the end with no inaccessible code

-- you can also use the binding operator to unencapsulate return data, so they are kind of inverses
-- a <- return "wow" === let a = "wow"

-- there are 2 proper use cases for return:
-- 1. we need an IO action that doesn't do anything
-- 2. we want to return something specfic as the last result of a do block, instead of the result value of its last action

-- a do block can have just one IO action, so some people like to have a 'then do return ()' so that the then do clause matches with the else do clause
-- see the reverse words example

-- .|||||||||| A FEW USEFUL FUNCS ||||||||||.

-- ! putStr [Char] - like putStrLn but no new line
-- ! putChar Char - outputs a character to the terminal
-- ! print (Show a) => a - shows the value then puts it on the screen: print = putStrLn . show
-- ghci actually uses print the echo the results of calls
-- ! getChar :: IO Char - reads a character from the input 
-- notably the characters stay in the buffer for the repeated calls of getChar
-- so it reads them 1 by  1, and then terminates at the spaces, effectively repeating character by character until the end of the first word
-- ! when {predicate} {IO action} - this is from Control.Monad, and it executes the IO action if the predicate is true but otherwise it does return ().
{-
import Control.Monad  
  
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main  
-}
-- ! sequence - this takes a list of IO actions and performs them sequentially, it combines multiple IO actions of the same type into one
-- sequence :: [IO a] -> IO [a]
-- rs <- sequence [getLine, getLine, getLine]
-- commonly sequence is used when we map functions like print or putStrLn over lists
-- map print [1,2,3,4] :: [IO String]
-- so we need to run sequence to turn them into an IO action
-- NOTE THAT IT COMBINES THE RETURN VALUES INTO A LIST
-- ! mapM - this basically combines that mapping and sequencing behaviour into one
-- ! mapM_ - this does the same as mapM but discards the return values so it just returns ()
-- ! forever - this takes an IO action and returns an IO action that will repeat the original forever, from Control.Monad
{-
import Control.Monad  
import Data.Char  
  
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  
-}
-- ! forM - this is like mapM, but takes the list first and then the function to map over the list which it sequences, this is useful because you can do stuff like:
{-
import Control.Monad  
  
main = do  
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors
-}
-- we didn't need to do color <- getLine and then return color
-- as color is the return value of the action getLine
-- but it makes it much clearer what we are doing
-- so that we can track that we input the list and output the chosen color into colors