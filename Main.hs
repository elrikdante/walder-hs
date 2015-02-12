-- |

module Main where

main :: IO()
main  = print $ "hello"
{-
In a pure lang
exception handling may be mimicked by
introducting a type to represent
computations that may raise an exception.
-}

data M a       = Raise Exception | Return a
type Exception = String

{-
Notice how our M a is a parametric type.
it maps over some arbitrary type a -
the constructors are compatible with mzero.

Raise is a type constructor mapped to some internal
exception represented by a value of type string

or it is a return , which specifies a successful
computation that *returned* some value of type a.

Eg: M Int
-}
data Term = Con Int
          | Div Term Term


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
