-- 99 Problems done by myself


-- 1] Find the last element of a list
mylast:: [a] -> a
mylast [] = error "empty list"
mylast [x] = x
mylast (x: xs) = mylast xs


-- Tail recursive style version: Stupid and perverse way to do it   
  



-- 2] Find the last but one element of a list

-- use if!!


onebutlast:: [a] -> a
onebutlast [] = error "empty list"
onebutlast (x : [xs]) = x
onebutlast (x : xs) = onebutlast xs 
                          


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






-- Boolean function OGM

boolean:: Bool -> String
boolean input = "ciao"


-- 3] Find the k-th element in a list. The first element in the list is number #1.

myfind:: [a] -> Int -> a
myfind (x : xs) 1 = x
myfind (x : xs) counter = myfind xs (counter - 1) 


-- 4] Find the number of elements in a list

mylength:: [a] -> Int
mylength [] = 0
mylength (x : xs) = 1 + (mylength xs)

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






















 

