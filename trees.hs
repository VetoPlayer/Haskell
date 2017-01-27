

-- Tree Example:

data Tree a = Empty | Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)



--3 Major classes (= Java interfaces)

-- Foldable: you have a container, a binary operator f and you want to apply f to all elements of the container
-- Foldable requires the implementation of a foldr function

-- foldr
-- Don't we specify the tfoldr type?? No, because it's already declared in the Foldable type.

-- Remark: It isn't anymore about Lists!!!! Be careful to consider every case.
-- To consider every case look for the data constructors

--Remark: When you implement an inteface function you don't need to specify its type, because it's already declared inside the interface definition
-- You need to implement foldr as minimal implementation to be foldable


-- Remark: this isn't a tail recursive implementation, for trees and tons of other data structures is the best approach


tfoldr f z Empty = z
tfoldr f z (Leaf x) = f x z
tfoldr f z (Node l r) = tfoldr f (tfoldr f z r) l


-- And then you make the trees an instance of Foldable:


instance Foldable Tree where
        foldr = tfoldr
        
         
-- Functor class

-- Tree are Functors:

-- Remember: you return the mapped value INSIDE the container

tfmap f Empty = Empty
tfmap f (Leaf x) = Leaf (f x)
tfmap f (Node l r) = Node (tfmap f l) (tfmap f r)
        
instance Functor Tree where
        fmap = tfmap
-- use the space and write it in the middle
          
 

-- Trees are Applicative Functors:


-- 1] You simply define how to concatenate 2 trees
tconc Empty t = t
tconc t Empty = t
tconc r l = Node r l


--2 ] You define a function that makes the concatenation happen with the rules you have defined above
tconcat t= tfoldr tconc Empty t

--3] How to apply a function over the trees and the concatenate the results
-- t in this case will be used as a tree of functions.
tconcatmap f t = tconcat (fmap f t)




instance Applicative Tree where
        pure = Leaf
        fs <*> xs = tconcatmap (\f -> fmap f xs) fs

-- You have a double fmap
-- For every element inside fs, xs is mapped and the result concatenated.        
-- since the data structure is a Functor, you can always use fmap
        





--let functiontree = Node (Leaf (+4)) (Leaf(*2))
-- let datatree = Node (Node (Leaf 5) (Leaf 7)) (Leaf 6)
-- functiontree <*> datatree
-- Node (Node (Node (Leaf 9) (Leaf 11)) (Leaf 10)) (Node (Node (Leaf 10) (Leaf 14)) (Leaf 12))
 

          
          
          
          
          
          
          
               
        
        
        
        
        
        
                







