import qualified Data.Map as M

type PersonName = String
type PhoneNumber = Integer
type BillingAddress = String



data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show)

--treeMapM:: (a -> State s b) -> Tree a -> State s (Tree b)
--treeMapM _ EmptyTree = return EmptyTree
--treeMap f (Node a l r) = do
  --      n' <- f a
    --    l' <- treeMapM f l
      --  r' <- treeMapM f r
        --return Node (n' l' r')
        
-- State Monad (Control.Monad.State)
-- haskell pure functional nature saves functions to have side effects (i.e., modifying a global state).
-- Function call just acts locally, computing and returning their results.
-- But what if we need to actually modifying the global state (stateful computations)
-- this need motivates the presence of the State Monad, that hides any edits to the global state, maintaining 
-- the explicit computation purely functional.












