-- Haskell 99 Problems


-- 1] Find the last element of a list
mylast:: [a] -> a
mylast [] = error "empty list"
mylast [x] = x
mylast (x: xs) = mylast xs


-- 2] Find the last but one element of a list

onebutlast:: [a] -> a
onebutlast [] = error "empty list"
onebutlast (x : [xs]) = x
onebutlast (x : xs) = onebutlast xs 


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


-- foldr version:

myfoldreverse:: [a] -> [a]
myfoldreverse list = foldr (\ x y -> y ++ [x]) [] list

--6] Find out whether a list is palindrome or not

isPalindrome:: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome list 
        | list == (myreverse list) = True
        | otherwise = False
        

-- 7] Flatten a nested list structure

data NestedList a = Elem a | List [NestedList a]



myFlatten:: NestedList a -> [a]

myFlatten (Elem a) = [a]
myFlatten (List []) =  []
myFlatten (List (x : xs)) = myFlatten x ++ myFlatten (List xs)

--myFlatten (List [(Elem 7), (Elem 8), List [(Elem 8)]])


-- version with concatMap:

myConcatFlatten:: NestedList a -> [a]
myConcatFlatten (Elem a) = [a]
myConcatFlatten (List []) = []
myConcatFlatten (List x) = concatMap myConcatFlatten x   

-- with foldr:

--foldflatter:: NestedList a -> [a]
--foldflatter (Elem x) = [x]
--foldflatter (List xs) = foldr (++) [] $ foldflatter xs


-- Remark: instead of xs you call a function returning xs 


--or with an accumulator function:

flatten4 = reverse . rec []
  where
  rec acc (List []) = acc
  rec acc (Elem x)  = x:acc
  rec acc (List (x:xs)) = rec (rec acc x) (List xs)


--8] Eliminate consecutive duplicates elements of a list

compress:: Eq a => [a] -> [a]

compress [] = []
compress [x] = [x]
compress (x : xs : xxs )
                | x == xs = compress ([x] ++ xxs)
                
                | otherwise = [x] ++ (compress ([xs] ++ xxs))
                
                
foldcompress:: Eq a => [a] -> [a]
foldcompress [] = []
foldcompress lst = foldr mylambda [] lst
      
                

-- can i define a boolen guard inside a \?

mylambda:: Eq a => a -> [a] -> [a]
mylambda x [] = x : []
mylambda x accum
                | x == (head accum) = accum
                | otherwise = (x : accum)


-- 9] Pack consecutive duplicates of a list into sublists

foldpack:: Eq a => [a] -> [[a]]
foldpack [] = []
foldpack lst = foldr pakker [[]] lst 



pakker:: Eq a => a -> [[a]] -> [[a]]
pakker x [[]] = [x : []]
pakker x lsts | x == (head (head lsts)) = (x : head lsts) : (tail lsts) 
              | otherwise =  [x] : lsts   



-- 10] Encode (encode '(a a a a b b)) -> [(4 a) (2 b)] 

encode:: Eq a => [a] -> [(Int, a)]
encode lst =
        let packedlst = foldpack lst 
        in map encoder packedlst 
        
encoder:: [a] -> (Int, a)
encoder [] = error "error"
encoder (x : xs) = (length (x : xs), x)


        
        
        
        
        
        
        
        
         





