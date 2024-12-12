import Distribution.Backpack.ComponentsGraph (dispComponentsWithDeps)
-- handling command line arguments is a neccessity if you want to make a script or application that runs on a terminal
-- haskell's standard library has some nice stuff for this

-- interactive programs are where the executable will allow the user to interact with it with prompts and getLines
-- this is not very good because you can't automate it, i.e. execute it with a batch script.
-- this is why we have arguments, where the user can tell the program what they want when they execute it.

-- from System.Environment
-- ! getArgs :: IO [String] - an IO Action that will get the arguments that the program was run with and have as its contained result a list with the arguments
-- ! getProgName :: IO String - an IO action that contains the program name

-- here's a demo:

{- import System.Environment (getArgs, getProgName)
import Data.List ()

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    let newArgs = map (++ ", ") args
    mapM_ putStr newArgs
    putStrLn "\nThe program name is:"
    putStrLn progName -}


-- firstly we make a 'dispatch association list', which associates the string arfs to their own IO actions

import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("view", view)
           , ("remove", remove)
           ]

-- notably we use Just action, so that it is of type Maybe IO ()
-- this is so that we do this because the lookup outputs a Maybe a
-- we aren't doing any error handling right now
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    putStr $ addLnNumbers contents

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    hPutStr tempHandle $ deleteLn (read numberString) contents
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

addLnNumbers :: String -> String
addLnNumbers contents = unlines $ zipWith appendNumber [0..] todoTasks
    where todoTasks = lines contents
          appendNumber n line = show n ++ " - " ++ line

deleteLn :: Int -> String -> String
deleteLn n contents = unlines $ delete (todoTasks !! n) todoTasks
    where todoTasks = lines contents