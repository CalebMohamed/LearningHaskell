-- Types are lables which allow us to reason about what values something may be
-- Types have their own labels called kinds.

-- you can find out the kind of something with :k in ghci

-- we find that :k Int -> Int :: *
-- and :k Maybe -> Maybe :: * -> *

-- so * must be a concrete type
-- and we see the type constructors kind declaration for Maybe where it takes a type and returns a type
-- so :k Maybe Int -> Maybe Int :: *

-- :k Either -> Either :: * -> * -> *
-- type constructors are curried so we can partially apply them - as seen when making Either a Functor
-- now we have terminology to say that Functor wants types of kind * -> *

-- check out this funky class
class Tofu t where
    tofu :: j a -> t a j

-- we can do some inference to work out what the kind of t must be:
-- as tofu is a function all the kinds of the expressions around the arrows must evaluate to *
-- so if we assume a :: *
-- j :: * -> *
-- as j a :: *
-- we know that (t a) turns j into a kind of * and j :: (* -> *)
-- so (t a) :: (* -> *) -> *
-- but we are subbing a into t to make this kind signature and a :: *
-- so t :: * -> (* -> *) -> *

-- look at this weird data type that constructs a value with concrete type from the type constructor and type in the wrong order!!!
-- we can derive what the parameters because of frankField type signature
-- (b a) :: * must hold
-- so we assume a :: *
-- => b :: (* -> *)

data Frank a b = Frank {frankField :: b a} deriving (Show)

-- Frank {frankField = Just "wat"} :: Frank [Char] Maybe
-- its so goofy that it reverses it
-- but it means Frank :: * -> (* -> *) -> *

instance Tofu Frank where
    tofu x = Frank x

-- tofu (b a) :: Frank a b -> Frank (b a), which is just the constructor

data Barry t k p = Barry { yabba :: p, dabba :: t k}
-- from yabba we assume p :: *
-- from dabba we assume k :: *
-- so t :: * -> *
-- so Barry :: (* -> *) -> * -> * -> *

-- so if we wanted to do some type-foo and make Barry instantiate the functor typeclass we need to partially apply the first two type parameters so we just have * -> *

instance Functor (Barry a b) where
    fmap f (Barry {yabba=x, dabba=y}) = Barry {yabba= f x, dabba=y}

-- we can do this because the 3rd of Barry's parameters has its own field and so can be transformed by the function without 'affecting the packaging of the box'