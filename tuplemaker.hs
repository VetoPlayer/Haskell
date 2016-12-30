
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




-- Constructor of list of tuples

encoder:: [a] -> (Int,a)
encoder [] = error "emptyy string"

encoder (x:xs) = (mylength (x : xs) , x)




mylength:: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs
