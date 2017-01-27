-- 99 Problems done by myself





-- Tail recursive style version: Stupid and perverse way to do it   
  




                          


-- i want to define a tail recursive version that stops itself at the right moment and returns the result
-- I would really like to have a "neutral" value for defining a generic variable, something like a neutral element 
-- maybe you can set in upon your first tail recursive call.
-- yes, the neutral value hugely depends upong the type you're using. You have to call the right value depending on what the function has to perform
--This concerns the tail recursive function's accumulator


--In this case it's trivial
lasthelper:: [a] -> a -> a

lasthelper [x] accum = accum    

lasthelper (x : xs) accum = lasthelper xs x

-- remark: lists can also be seen as x : xs : xxs    



-- 3] Find the k-th element in a list. The first element in the list is number #1.

myfind:: [a] -> Int -> a
myfind (x : xs) 1 = x
myfind (x : xs) counter = myfind xs (counter - 1) 


-- 4] Find the number of elements in a list

mylength:: [a] -> Int
mylength [] = 0
mylength (x : xs) = 1 + (mylength xs)


-- foldl implentation:

myfoldlength:: [a] -> Int
myfoldlength l= foldl (\x y -> x + 1) 0 l



-- 5] Reverse a list

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x :xs) = (myreverse xs) ++ [x]

--6] Find out whether a list is palindrome or not

isPalindrome:: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome list 
        | list == (myreverse list) = True
        | otherwise = False
        
        
-- 7] Flatten a nested list structure.

--TODO Review! Importante excercice! for this one you have to define an ad hoc type and you deal with list of lists
-- We need to define a new data type because lists in haskell are homogeneous:

--data NestedList a = Elem a | List [NestedList a]
--myflatten:: NestedList a -> [a]
--myflatten (Elem a) = [a]
--myflatten (List (x : xs)) = (myflatten x) : (myflatten xs)
--myflatten (List []) = []

--Remark: when defining the input of a function as some type or something with an operator, you NEED to use parentheses

--TODO review concatMap, It's really important!!
-- TODO reimplement this excercice with a concat map versione and a foldr version




--7] Eliminate consecutive duplicates of list elements. 
-- It can be easily done with a simple fold

compress:: Eq a => [a] -> [a]
compress lst = foldr isConsecutiveEqual [] lst  





-- omg i would really like to have an if inside my \ function!!
-- I think the only solution in this case is to do define a new proper function and use it

isConsecutiveEqual:: Eq a => a -> a -> Bool
isConsecutiveEqual x y | x == y = True
                       | otherwise = False

-- what about using concatMap??????? TODO





-- Let's reimplement the fold, it's a too important function

myfold:: (a -> a -> a) -> a -> [a] -> a
myfold f accum [] = accum
myfold f accum (x : xs) = myfold f (f accum x) xs    

-- Second Exercitation

--9] Pack function
-- Pack function: Pack consecutive duplicates of list elements into sublists.

pack:: Eq a => [a] -> [[a]]
pack [] = []
pack input = reverse (packhelper input [[]] [])

-- What you are doing here is calling an (obviously recursive) function with more input paramenters than the original function


--remember: it's always better to see a list as (x: xs) if you want to play with it

-- you have 3 arguments: input you are consuming, !!!the partial result!!!, a temporal variable storing the input read
packhelper:: Eq a => [a] -> [[a]] -> [a] -> [[a]]
packhelper [] acc tmp = tmp : acc
packhelper (x : xs) acc [] = packhelper xs acc [x]
packhelper (x : xs) acc (y : ys)
        | x == y = packhelper xs acc (x : tmp) -- if x == y you want to pack them together. == is the reason why you want Eq a => a
        | otherwise = packhelper xs (tmp : acc) [x] -- otherwise you store the 'old' list and make a new one
        where tmp = y : ys



-- you have it reversed because you unroll you input sequentially and make it cons with the [], so your first element will be the last
-- Remark: in all recursive function you'll have for sure a base case in which you don't perform the recursive call

--- 10] Encode function:
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive -- duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. 

-- For instance:
--(encode '(a a a a b c c a a d e e e e))
--((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

encode:: Eq a => [a] -> [(Int,a)]
encode input [] = encodehelper (pack input) []


-- input, accumulator --
encodehelper:: Eq a => [[a]] -> [(Int, a)] -> [(Int, a)]
encodehelper [] accum = accum
encodehelper (x : xs) accum = encodehelper xs ((encoder x) : accum)



encoder::Eq a => [a] -> (Int,a)
encoder [] = error "emptyy string"

encoder (x:xs) = (length (x : xs) , x)





--remark: the accumulator has tipically the same type of the result


-- in Haskell "Tail recursive things are just function with an 'accumulator' argument storing the partial result"




















 

