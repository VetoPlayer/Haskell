        
data MyMaybe a = MyNothing | MyJust a deriving (Show)

        
-- a is really general and can be composed of any kind of type!!! If you want you can make a a Tree, a List or another MyMaybe
-- You cannot choose however differents things: once you choose a type, the expression that you write down must be homogeneous

-- Make Maybe foldable:

mfoldr f z MyNothing = z
mfoldr f z (MyJust x)= f x z      


instance Foldable MyMaybe where
        foldr = mfoldr
        
        
-- Maybe is a Functor:



maybemap f MyNothing = MyNothing
maybemap f (MyJust x) = MyJust (f x)



instance Functor MyMaybe where
        fmap = maybemap
          

   
-- Maybe is an Applicative Functor

instance Applicative MyMaybe where
        pure = MyJust
        
        MyJust f <*> m = fmap f m
        MyNothing <*> _ = MyNothing
        
        
        
        
        
