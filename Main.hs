-- |

module Main where

main :: IO()
main  = print $ "hello"
{-1
In a pure lang
exception handling may be mimicked by
introducting a type to represent
computations that may raise an exception.
-}
type Exception = String
data Term      = Con Int
               | Div Term Term
{-2
Notice how our M a is a parametric type.
it maps over some arbitrary type a -
the constructors are compatible with mzero.

Raise is a type constructor mapped to some internal
exception represented by a value of type string

or it is a return , which specifies a successful
computation that *returned* some value of type a.

Eg: M Int
-}
data M a       = Raise Exception | Return a

{-3
State

Introduce state by creating a type synonym for M
data M a = Raise Exception
         | Return a

here, M has kind * (no unfilled parameters)

Furthemore its a data type, not a constructor.  So we
can't use it as a constructor until we do this:

type M a = State -> (a, State)

now, we have a constructor which takes
a parameter of type a (kind *)
and produces a function that takes a state param.

and reutns a value of kind * which is a pair
of the enclosed type parameter a, and the state the

computation was initialised with.
-}

type M a       = State -> (a, State)
type State     = Int
-- We will use this to count the number of
-- Div operations performed.



runM            :: Show a => M a -> a
runM (Return a) = a
runM (Raise  a) = Prelude.error $ "Error" ++ a

eval           :: Term -> M Int
eval (Con a)   = Return a
eval (Div t u) =
  case eval u of
   Raise  e    -> Raise e
   Return a    -> case a == 0 of
     True  -> Raise "Divid By Zero"
     _     -> case eval t of
       Raise  e -> Raise e
       Return b -> Return (b `div` a)


answer,error :: Term
answer = (Div (Div (Con 1972)(Con 2))(Con 23))
error  = (Div(Con 1)(Con 0))
