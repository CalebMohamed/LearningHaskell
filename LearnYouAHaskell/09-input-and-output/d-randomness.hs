-- Randomness in haskell isn't as easy as random functions aren't pure

-- System.Random module!
-- random :: (RandomGen g, Random a) => g -> (a, g)
-- ! RandomGen - typeclass for types that can act as sources of randomness
-- ! Random - typeclass is for things that can take on random values, i.e. Booleans, Ints, but not functions.

-- So firstly we need a generator g, System.Random has StdGen
-- to manually make one you need:
-- ! mkStdGen :: Int -> StdGen - It takes an Int seed and returns a StdGen.

-- random (mkStdGen 100) throws an error, as random doesn't know the type of the random variable
-- random (mkStdGen 100) :: (Int, StdGen) works

{- import System.Random

myNotReallyRandomNumber :: (Int, StdGen)
myNotReallyRandomNumber = random (mkStdGen 100) -}

-- as expected to generate multiple values, we chain the generator outputs:

{- threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen
        (secondCoin, newGen') = random newGen
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin) -}

-- ! randoms :: (RandomGen g, Random a) => g -> [a] - takes a random generator and returns an infinite list of random numbers.
-- possible definition:
{- randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = 
    let (value, newGen) = random gen 
    in value:randoms' newGen -}

-- for finite randoms
{- finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> [[a], g]
finiteRandoms - gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
    (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in (value:restOfList, finalGen) -}

-- ! randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g) - this takes a range in the form of a pair and a random generator which then returns a random value in the range and another generator

-- ! randomRs - like randomR crossed with randoms, producing an infinite list of values

-- ! getStdGen :: IO StdGen - gets a good random generator from the OS
-- careful, this generator is the global generator in the OS and asking twice will get the same twice

-- ! newStdGen - splits the current random generator in two, updating the global random generator with one of them and encapsulating the other as its result

-- classic guessing game
{- import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    askForNumber gen  
  
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number  
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen -}

-- ! reads - like read but doensn't crash on erroneous input instead returing an empty list, and a singleton list with a typle with the desired value as one component and a string with what it din't consume as the other.

-- another implementation
-- but the whole main function is recursive in this one
{- import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number  
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        newStdGen  
        main -}