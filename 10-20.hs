--11]
--12]
--13]
--14] Duplicate the elements of a list
duplicate:: [a] -> [a]
duplicate [] = []
duplicate lst = foldr (\x y -> (x : x : y)) [] lst 



--15] Replicate the elements of a list a given amount of times
--replicate [1,2,3] 2 ---> [1,1,2,2,3,3]

myreplicate:: [a] -> Int -> [a]
myreplicate [] _ = []
myreplicate (x : xs) num = (repl x num []) ++ (myreplicate xs num)
                        where
                                repl x 0 accum =  accum
                                repl x num accum = repl x (num - 1) (x : accum)
                                   

-- Defining lambdas in haskell



-- Closures: Adder by 2

mkAdder :: Int -> (Int -> Int)
mkAdder y = \x -> x + y 

--16] Drop every N-th element of a list

-- recursive version
dropper:: [a] -> Int -> [a]
dropper (x : xs) num 
                | num == 1 = xs
                | otherwise = x : dropper xs (num - 1)
                
-- 17] Split a list into two parts, the length of the first list is given.

-- Tail recursive version

splitter:: [a] -> Int -> [[a]]
splitter lst leng = hsplit lst leng []
                where
                hsplit (x : xs) leng firstList | leng > 0  = hsplit xs (leng - 1) (firstList ++ [x])
                                               | otherwise = firstList : [xs]
                
                


--18] Extract a slice from a List

slicer:: [a] -> Int -> Int -> [a]
slicer lst first last = hslice lst 0 (first - 1)  (last - 1) []
                    where
                    hslice (x : xs) i first last accum | (i >= first) && (i <= last) = hslice xs (i + 1) first last (accum ++ [x])
                                                       | i > last = accum
                                                       | otherwise = hslice xs (i + 1) first last accum  

--19] Rotate a list N places to the left


rotate:: [a] -> Int -> [a]
rotate lst num = let splitted = splitter lst num
                 in  (head (tail splitted)) ++ (head splitted)



-- 20] Remove the K-th element from the list

remover:: [a] -> Int -> [a]
remover [] index = []
remover (x : xs) index | index == 1 = remover xs (index - 1)
                       | otherwise = [x] ++ (remover xs (index - 1)) 




-- For defining "if" inside lambdas in Haskell you can use the usual construct if ... then ... else ...

-- Where Syntax:


bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0
          



















