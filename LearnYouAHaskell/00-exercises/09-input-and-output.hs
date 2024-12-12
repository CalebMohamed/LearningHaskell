{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
import System.Environment
import System.Directory
import System.IO
import Data.List

main = do
    args <- getArgs
    if head args == "-n"
        then mapM_ (putStr . (++ " ")) $ tail args
        else mapM_ putStrLn args

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
-- lottery :: StdGen -> [Int]
-- lottery gen = undefined