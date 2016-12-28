
-- Simple constructor of tuples.
tuplebuilder:: a -> a -> (a, a)
tuplebuilder first second = (first, second)


-- Constructor of list of tuples

encoder:: [a] -> (Int,a)
encoder [] = error "emptyy string"

encoder (x:xs) = (mylength (x : xs) , x)




mylength:: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs
