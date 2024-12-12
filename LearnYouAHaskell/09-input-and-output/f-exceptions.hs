-- When handling errors, Haskell's Maybe and Either algebraic data types are very useful...
-- as they prevent accidently using a convention for error handling as a valid value like with -1 in C.

-- despite the safety from the compiler these datatypes afford us, haskel still has support for exceptions, because they make more sense in IO contexts.
-- this is because when dealing with the outside world a lot can go wrong, like trying to open a locked file, or one that doesn't exist.

-- But pure code can also throw exceptions, but they can only be caught in the IO part of the code, because we don't actually know when or if any pure code will be evaluated. 
-- consequently our IO parts can get bloated with logic which we don't want

-- the solution is to prefer the use of Maybe and Either to represent results that may have failed, even though you can use exceptions

-- --

-- Consider the following
-- it will throw an exception if the first argument is not a value file path
{- import System.Environment  
import System.IO  
  
main = do (fileName:_) <- getArgs  
          contents <- readFile fileName  
          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"   -}

-- there are two main way we could deal with this:
-- preemptively with ! doesFileExist - an IO Bool Action that takes as a parameter a file path
-- or retroactively with...
-- ! catch :: IO a -> (IOError -> IO a) -> IO a - a function that takes the first action which may err, and a handler function which takes the error of the first function and returns an IO action if it fails.
-- the error type is implemented depeending on te implementation of the language itself so it cannot be pattern matched against just like IO something.

{- import System.Environment  
import System.IO  
import System.IO.Error  
  
main = toTry `catch` handler  
  
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e = putStrLn "Whoops, had some trouble!"   -}

-- now the power of exeption handling in all languages is that you can choose to handle some errors and pass on others to end the program:

{- import System.Environment  
import System.IO  
import System.IO.Error  
import Control.Exception
  
main = toTry `catch` handler  
  
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e   -}

-- ! isDoesNotExistError - a predicate function which takes an IOError and returns if the error was due to a file not existing
-- ! ioError - takes an IOError and produces an IO action that will throw it again

-- IOError predicates:
-- ===================
-- ! isAlreadyExistsError
-- ! isDoesNotExistError
-- ! isAlreadyInUseError
-- ! isFullError
-- ! isEOFError
-- ! isIllegalOperation
-- ! isPermissionError
-- ! isUserError

-- self-explanatory except for isUserError
-- ! userError :: String -> IOError - to make a custom exception although its much better to use Maybe an Either

-- !!! MAKE SURE TO RETHROW EXCEPTIONS IF THEY DON'T MATCH CRITERIA !!!

-- System.IO.Error exports some functions which allow us to query the error for extra info, like the filePath that doesn't exist
-- all of these functions start with ioe (IOError abbrv)

-- ! ioeGetFileName :: IOError -> Maybe FilePath - pretty self explanatory

import System.Environment  
import System.IO  
import System.IO.Error  
import Control.Exception
  
main = toTry `catch` handler  
  
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = 
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e  