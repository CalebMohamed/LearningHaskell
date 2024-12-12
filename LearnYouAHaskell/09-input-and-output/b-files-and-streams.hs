-- getContents :: IO String - an IO action that reads everything from the standard input until it encounters an End-of-File character.
-- its cool because its lazy
-- its useful for something called piping where you take the output of one program as a text file and then input it into another
-- in GNU 'cat' takes a text file and just prints out its contents to the terminal
-- we can then pipe it to a program with cat file.txt | ./program

-- this the program used in the tutorial
-- it goes line by line and converts it into the uppercase form forever
{- import Control.Monad  
import Data.Char  
  
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  
-}

-- but we can write this a lot more simply with the getContents action

{-
import Data.Char

main = do
    putStrLn "--- ready | press Ctrl+Z Enter to exit ---"
    contents <- getContents
    putStr (map toUpper contents)
-}

-- now becaus getContents is lazy it won't read the whole content and then convert it and print it, but it will print out the capslocked version as it reads and will only read the next line when it needs to.
-- interestingly just like ghci, you exit with the EOF character, for this windows terminal its Ctrl+Z Enter

{-
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input = 
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
-}

-- here we are showing the power of the do block
-- we make it as small as possible and take all the functional parts out
-- lines splits a string by its lines into a [String]
-- unlines reverses this
-- we uses a lambda function for the filter

-- the pattern of getting some string from the input, transforming it with a function and outputting is so common that there exists a function which makes it even easier
-- ! interact - takes a function of type String -> String, and returns an IO action that will take some input run the func and then print.

{-
main = interact shortLinesOnly 
  
shortLinesOnly :: String -> String  
shortLinesOnly input = 
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
-}

-- EVEN SHORTER!!!

{-
main = interact $ unlines . filter ((<10) . length) . lines  
-}

-- lets make an interaction where the program responds to whether the line is a palindrome

-- main = do
--     putStrLn "running | ctrl+z enter to exit"
--     interact respondPalindromes

{-
respondPalindromes :: String -> String
respondPalindromes =
    unlines . map (\xs -> if isPalindrome xs 
        then "~ " ++ xs ++ "! palindrome"
        else "~ not a palindrome"
    ). lines
    where isPalindrome xs = xs == reverse xs
-}

-- reading from files is very similar to reading from and writing to the stdin and stdout - which are the terminal

{- import System.IO

 main = do
    handle <- openFile "file.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle -}

-- ! openFile :: FilePath -> IOMode -> IO Handle - the type explain what it is doing (FilePath String synonym)
-- we can store the file handle object in the handle with the <- binding
-- then we can pass the handle to other functions to do stuff

-- ! hGetContents - takes a handle and returns an IO String which holds the contents of the file, like getContents for stdin
-- its also lazy, yay!

-- ! hClose - takes a handle and returns an IO action that closes the file

-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode, its just an enumeration that represents what we want to do with the opened file, note that there are no spaces, its not an IO Action type

-- ! withFile - this takes a file path, IOMode, and function of the form (Handle -> IO a), return an IO Action with the result of the function, and finally close the file

-- so we could write the previous code as:

{- main = do
    withFile "file.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents) -}

{- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result -}

-- hGetLine, hPutStr, hPutStrLn, hGetChar all work as expected
-- ! readFile - is a special case of withFile that just takes a filePath and outputs an IO String of the contents.

-- ! writeFile - this takes a file path and string and outputs an IO () which will overwrite the contents of the file with the String

-- ! appendFile - this is just like writeFile but adds the string to the end

{- import System.IO (openFile, IOMode (AppendMode), hPutStr, hFlush, hClose)
import Control.Monad ( when )

main = do
    putStrLn "--oO running Oo--"
    contents <- readFile "toDoList.txt"
    putStr contents
    mainLoop
    contents <- readFile "toDoList.txt"
    putStr contents
    
mainLoop = do
    l <- getLine
    when (l /= "") $ do
        handle <- openFile "toDoList.txt" AppendMode
        hPutStr handle ("\n- " ++ l)
        hFlush handle
        hClose handle
        mainLoop -}

-- when binding the output of hGetContents handle to a name, it doesn't read the whole thing - its IO lazy
-- consequently using withFile or readFile which use hGetContents are also lazy
-- Instead a metaphorical 'pipe' is connected, which reads the file or accesses the disk in buffers:
-- for text files, the buffer lize is one line, hence the behaviour seen before, for binary files its 'block-buffering' which is chunk sizes defined by the os.

-- you can set the buffering method with hSetBuffering
-- ! hSetBuffering handle BufferMode - this returns an IO action that sets the buffering.
-- NoBuffering - 1 character at a time
-- LineBuffering - self explanatory
-- BlockBuffering (Maybe Int) - Nothing allows OS defined, Just Int defines the size of the chunk in bytes

-- Reading files in bigger chunks can help to minimize disk access or when our file is actually a slow network resource.

-- ! hFlush handle - takes a handle and returns an IO action that will flush the buffer of the file associated with the handle.
-- when doing line-buffering the buffer is flushed after each line, and the reading or writing mechanism reports all the data so far, we can use hFlush to force that reporting, and make it available to other programs running at the same time.

-- an example of some code that will delete allow the user to delete an item from the todolist

import System.IO  
import System.Directory  
import Data.List  
  
main = do  
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"  
    numberString <- getLine  
    let number = read numberString  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"

-- ! openTempFile - this takes a path to a temporary directorary and a template for the file name (which is followed by a bunch of random characters)
-- it then returns a pair of the name and handle
-- its better to make a new temp file and then delete the first so you are sure you aren't acidentally overwriting another file

-- ! removeFile - this takes the file path and deletes it
-- ! renameFile - this takes the old file path and new one and renames it