--3 Major classes (= Java interfaces)

-- Foldable: you have a container, a binary operator f and you want to apply f to all elements of the container
-- Foldable requires the implementation of a foldr function

-- Tree Example:

data Tree a = Empty | Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)


-- foldr
-- Don't we specify the tfoldr type??

-- Remark: It isn't anymore about Lists!!!! Be careful to consider every case.
-- To consider every case look for the data constructors


-- Remark: this isn't a tail recursive implementation, for trees and tons of other data structures is the best approach


tfoldr f z Empty = z
tfoldr f z (Leaf x) = f x z
tfoldr f z (Node l r) = tfoldr f (tfoldr f z r) l


-- And then you make the trees an instance of Foldable:


instance Foldable Tree where
        foldr = tfoldr
        
        
        
data MyMaybe a = MyNothing | MyJust a


-- a is really general and can be composed of any kind of type!!! If you want you can make a a Tree, a List or another MyMaybe
-- You cannot choose however differents things: once you choose a type, the expression that you write down must be homogeneous

-- Make Maybe foldable:

mfoldr f z MyNothing = z
mfoldr f z (MyJust x)= f x z      


instance Foldable MyMaybe where
        foldr = mfoldr
        
        
-- Functor class

-- Tree are Functors:

-- Remember: you return the mapped value INSIDE the container

tfmap f Empty = Empty
tfmap f (Leaf x) = Leaf (f x)
tfmap f (Node l r) = Node (tfmap f l) (tfmap f r)
        
instance Functor Tree where
        fmap = tfmap
        
          
-- Applicative Functors: 

-- 2 Kind of operations: pure and <*>


-- pure
          
-- <*>






-- concat:


myconcat:: Foldable f => f [a] -> [a]

myconcat list = foldr (++) [] list


-- You have a concatenation for lists only, NOT for a generic type


-- concatMap: you apply map over everysingle element of the function and then you concatenate the results:

-- map (\x -> x + 1) [[1],[2,3]] --->[[2],[3,4]] -> [2,3,4]
myconcatMap f l = concat (map f l)





-- List are instances of applicative!!

--instance Applicative [] where
--              pure x = []
--               fs <*> xs = concatMap (\f -> map f xs)
               
               
-- [(+1),(*2)] <*> [1,2,3]


   
-- Maybe is an Applicative Functor

instance Applicative MyMaybe where
        pure = MyJust
        
        Just f <*> m = fmap f m
        Nothing <*> _ = Nothing


-- Trees are an applicative functor


-- Fist things at Fist: we need to define the tree concatenation:

-- concatenation has 2 input: 2 trees you want to concatenate

tconc Empty t = t
tconc t Empy = t
tconc t1 t2 = Node t1 t2

-- the concatenation is just a fold over the container of what you mean by concatenation:
tconcat t = tfoldr tconc Empty t
tconcatmap f t = tconcat (tmap f t)


--Ok!

instance Applicative Tree where
        pure = Leaf
        fs <*> xs = tconcatmap (\f -> tmap f xs) fs -- why fs??? i would say xs!!









          
          
          
          
          
          
          
          
               
        
        
        
        
        
        
                







