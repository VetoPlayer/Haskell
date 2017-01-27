-- 22] Create a list containing all integers in a given range
ranger:: Int -> Int -> [Int]
ranger first last = [first .. last]


-- 28] Sort a list of lists according to the length of sublists

lsort:: [[a]] -> Int -> [a]
lsort (x : xs) leng = 
