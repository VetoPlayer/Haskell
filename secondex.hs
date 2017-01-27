-- Tommasini's Second Exercitation

data Point = Point Int Int deriving (Show)

getx (Point x _) = x
gety (Point _ y) = y


-- For complex types you can automatically define accessor:

data Person = Person {
                firstname :: String
                , secondname :: String
                , age:: Int
                } deriving (Show)
                
                
-- Towards Monads:

data Tree a = Empty | Node (Tree a) (Tree a) | Leaf a deriving (Show)

-- Trees are instance of Eq, Functors, Foldable, Applicative, Monads

-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b


instance (Eq a) => Eq (Tree a) where
        Empty == Empty = True
        Leaf a == Leaf b = a == b
        Node x y == Node a b = (x == a) && (y == b)
        _ == _ = False


instance Foldable Tree where
        foldr = tfoldr


tfoldr _ accum Empty = accum
tfoldr f accum (Leaf x) = f x accum
tfoldr f accum (Node left right)= tfoldr f (tfoldr f accum right) left

instance Functor Tree where
        fmap = treemap
        
-- Functor f => (a -> b) -> f a -> f b     

treemap f Empty = Empty
treemap f (Leaf a) = Leaf (f a)
treemap f (Node left right) = Node (treemap f left) (treemap f right)


-- Applicative

-- In order to be Applicative, a type needs to be a Functor

-- Applicative f => f (a -> b) -> f a -> f b

instance Applicative Tree where
       pure a = Leaf a
       fs <*> xs = tconcatmap (\f -> f) 

treeapply 

         

-- Tree Concatenation definition

-- Concatenates 2 Trees
tconcat:: Tree a -> Tree a -> Tree a
tconcat Empty x = x
tconcat x Empty = x
tconcat x y = Node x y

-- Recursively Concatenates trees (or better, trees within a tree)
tconcatrec t = foldr tconcat Empty t

-- ConcatMap Implementation for trees:

-- concatMap:: Foldable t => (a -> [b]) -> t a -> [b]



tconcatmap funct tree =  tconcatrec (fmap funct tree)











         
         
         
         
         





        
                

