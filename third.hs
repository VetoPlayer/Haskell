-- Implement Fold
myfold:: (a -> a -> a) -> a -> [a] -> a
myfold f z [] = z
myfold f z (x: xs) = myfold f (f z x) xs

-- Implement the true Fold!:

myfoldl:: => (b -> a -> b) -> b -> t a -> b 
myfoldl f z (container x) = 


--Question of the questions!
-- How can I go on with the recursive call of the fold if i have a generic structure? I would relly like to have a sort of "cons" operator (defined
-- only for lists) for a generic type.

-- Answer by myself: For this reason t is restricted to be foldable: the container type itself has to offer a way in which it can be Folded






-- Implement Map




-- Implement Map with a Fold
-- f == List constructor
--tfold:: (f a ->f a ->f a) -> a -> f a -> f a
--tfold func z 



-- Fold inverting a list

-- Implement map with a fold: list case!

listfold:: ([a] -> a -> [a]) -> [a] -> [a] -> [a]
listfold f z (x : xs) = listfold f (f z x) xs



-- Implement Filter with a Fold




