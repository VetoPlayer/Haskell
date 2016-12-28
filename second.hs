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
























        
        
        
        
        
