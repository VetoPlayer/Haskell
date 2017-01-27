-- Monads examples.


-- All you need to do with >>= (a.k.a. Bind) is to extract the value out of its context and feed the function with it.

-- You have m a and a function that simply wants a : (a -> m a)

-- >>= Implementation for Maybe:

maybeBind:: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing f = Nothing
maybeBind (Just x) f = f x -- Here you are doing the key point of monads: you are extracting x from Maybe to feed it to the function



-- Pierre Example (From learnyouahaskell.com)

type Birds = Int
type Pole = (Birds, Birds)


-- function that makes num birds land on the right side of the pole:

oldLandRight:: Birds -> Pole -> Pole
oldLandRight num (left, right) = (left, right + num)

-- function that makes num birds land on the left side of the pole:

oldLandLeft:: Birds -> Pole -> Pole
oldLandLeft num (left, right) = (left + num , right)

-- Birds fly away when you give negaive numbers to these functions


-- function that allows you to write firstly the parameters, then the function taking them.
-- With this you can see a lot better what's appening to a certain variable:
-- (0,0) -: landLeft 1 -: landRight 2
x -: f = f x 


-- With more than three birds of difference between left and right, Pierre will fall.
-- BUT if you have something like this, you'll never notice!!
-- (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)  which gives you (0,2)

-- We want functions to be able to FAIL if there are too many birds in the wrong way

landLeft:: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
        | abs ((left + n) - right) < 4 = Just (left + n, right)
        | otherwise                    = Nothing
        
        
landRight:: Birds -> Pole -> Maybe Pole
landRight n (left, right)
        | abs (left - (right + n)) < 4 = Just (left, right + n)
        | otherwise                     = Nothing

-- Instead of returning a Pole, these functions are now returning a Maibe Pole
-- Now the notation -: simply isn't enough. We have lost the possibility to express landings in a 'sequential' way.
-- But we can do it with >== !!


--     ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)  ----> Nothing


-- function that makes Pierre slip no matter what:

banana:: Pole -> Maybe Pole
banana _ = Nothing


--return (0,0) >>= landLeft 1 >>= banana >>= landRight 1


-- In this case since the banana is a 'constant' function that doesn't read the previous pole, you cound substitute it with >> Nothing, which is equivalent.

-- >>= banana ≡ >> Nothing 



--return (0,0) >>= landLeft 2 >>= landRight 2 >>= landLeft 1

-- ≡
routine:: Maybe Pole
routine = do
        start <- return (0,0)
        first <- landLeft 2 start
        second <- landRight 2 first
        landLeft 1 second




--lists are monads:

instance Monad [] where
        return x = [x]
        xs >>= f = concat $ map f xs
        fail _ = []







 

















