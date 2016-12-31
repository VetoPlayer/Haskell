-- hof= Higher Order Functions Excercices and thoughts




-- Simple constructor of tuples.
tuplebuilder:: a -> a -> (a, a)
tuplebuilder first second = (first, second)


-- Map

myMap:: (a -> a) -> [a] -> [a]
myMap f [] = []
myMap f (x : xs) = (f x) : (myMap f xs)

-- Filter

myFilter:: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x : xs)  | (f x) = x : (myFilter f xs)
                     | otherwise = myFilter f xs
                     

--Fold

myFold:: (a -> a -> a) -> a -> [a] -> a
myFold f accum []= accum
myFold f accum (x : xs)= myFold f (f x accum) xs

-- Implement the true Fold!:

--myfoldl:: => (b -> a -> b) -> b -> t a -> b 
--myfoldl f z (container x) = 


-- Implement Filter with a Fold



--Question of the questions!
-- How can I go on with the recursive call of the fold if i have a generic structure? I would relly like to have a sort of "cons" operator (defined
-- only for lists) for a generic type.

-- Answer by myself: For this reason t is restricted to be foldable: the container type itself has to offer a way in which it can be Folded
















-- Parsing a list with a foldl:
--TODO Review it!! It's relly important!!!!
--foldl:: Foldable t => (b -> a -> b) -> b -> t a -> b
--foldl (\ x y -> x ++ [y]) [] [1,2,3]

--foldr:: Foldable t => (a -> b -> b) -> b -> t a -> b
--foldr (\x y -> x : y) [] [1,2,3]


-- Reversing  a list with foldl:

mylreverse:: [a] -> [a]
mylreverse lst = foldl (\x y -> y : x) [] lst


myrreverse:: [a] -> [a]
myrreverse lst = foldr (\x y -> y ++ [x]) [] lst







-- Implement concat:


myconcat:: Foldable t => t [a] -> [a]
myconcat foldablecontainer = foldl (++) [] foldablecontainer 



-- concat:


myconcat:: Foldable f => f [a] -> [a]

myconcat list = foldr (++) [] list







-- The composition of concat and map is called concatMap: you first map a certain function over the generic Foldable container of lists and then
-- you concatenatethe results:


myconcatMap :: Foldable t => (a -> [b]) -> t a -> [b] 

myconcatMap f foldable = concat (map f foldable)


