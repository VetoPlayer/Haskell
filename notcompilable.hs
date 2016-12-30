
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




