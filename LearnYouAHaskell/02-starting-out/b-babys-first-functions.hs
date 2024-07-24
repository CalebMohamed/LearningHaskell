doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y 
doubleSmallNum x = if x > 100
                    then x
                    else doubleMe x

-- the ' is a valid character in haskell so it is used to denote strict (non-lazy) or slightly modified functions
doubleSmallNum' x = (if x > 100
                    then x
                    else doubleMe x) + 1

definition = "A function without parameters, like 5"