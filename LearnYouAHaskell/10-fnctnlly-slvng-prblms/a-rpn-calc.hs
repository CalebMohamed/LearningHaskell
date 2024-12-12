import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Char

rpnSymbols :: [Char]
rpnSymbols = "+-*# "

main = do
    args <- getArgs
    let collapsedArgs = unwords args
    putStr $ collapsedArgs ++ " -> "
    if isStringInRPN collapsedArgs
        then print $ solveRPN collapsedArgs
        else putStrLn "Not in RPN format."
    

isStringInRPN :: String -> Bool
isStringInRPN = foldr checkChar False
    where checkChar x acc = isDigit x || (x `elem` rpnSymbols) || acc

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunc [] . words
    where foldingFunc (x:y:ys) "+" = (x+y):ys
          foldingFunc (x:y:ys) "-" = (x-y):ys
          foldingFunc (x:y:ys) "*" = (x*y):ys
          foldingFunc (x:y:ys) "#" = [0]
          foldingFunc xs numberString = read numberString:xs