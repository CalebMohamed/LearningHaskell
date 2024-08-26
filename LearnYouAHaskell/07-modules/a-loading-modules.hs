{-
 modules are collections of related functions, types and typeclasses.
 a haskell program is a collection of modules where the main module loads up the other modules and then uses their functions to do something.
 modules are good for reusability general applicability, readability and maintainability.
-}

{- 
 The Standard Library is split into modules e.g. one for list manipulation, concurrent programming, complex numbers, and so on.
 All the functions types and typeclasses so far have been from the Prelude module, which is default.
-}

{-
 Importing modules is quite straight forward, you simply right import <module name> before defining any functions (so typically at the top).
-}

-- this module has a lot of functions for working with lists, importing it like this brings all its functions into the global namespace
import Data.List ()

-- you can also bring modules into the global scope in ghci by doing :m + <module name>

-- to selectively import only certain functions you can do import <module name> (foo, bar)
-- import all but a few you can do: import <module name> hiding (foo)

-- what do we do if we want to use functions from modules with the same names, Data.Map provides a data structure for looking up values by keys but it defines its own filter, which conflicts with Prelude's
-- to solve this we can use the 'qualified' keyword to make up specify the modulename.foo 
-- import qualified Data.Map
-- then to call the function: Data.Map.filter

-- that's kind of long so you can name them with the 'as' keyword
-- import qualified Data.Map as M
-- then to call the function: M.filter