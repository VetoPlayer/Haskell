


-- Muxel

data Muxel a = Ls [a] | Nm Integer | End deriving Show
type Unsorted a = [Muxel a]




-- Replicate: takes a list and returns only the elements repreated al least twice of that list.

rep :: Eq a => [a] -> [a]

rep lst = rephelp lst []

rephelp::Eq a => [a] -> [a] -> [a]
rephelp [] accum = accum
rephelp (x : xs) accum = if (elem x xs) && (not(elem x accum))
                          then rephelp xs (x : accum)
                          else rephelp xs accum
                          
--sumlist:: Num a => [a] -> [a] -> [a]


listAbsolute:: (Num a, Ord a) => [a] -> [a]
listAbsolute [] = []
listAbsolute (x : xs )
                | x > 0 = x : (listAbsolute xs)
                | otherwise = (-x) : (listAbsolute xs)  
                
                
data NestedList a = List [NestedList Int] | EmptyList
