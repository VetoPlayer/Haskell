module FirstTrial where

-- Lenght Function

-- a represents a list of elements of type a of any kind
mylength:: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs


-- Tommasini Excercices
-- Reverse Function

myReverse:: [a] -> [a]

myReverse [] = []
myReverse(x : xs) = myReverse xs ++ [x]

-- Empty Function telling whether a list is empty or not

empty:: [a] -> Bool

empty [] = True
empty (_ : xs) = False


-- isPositive function: takes an Int as input and returns True if it's strictly positive, False otherwise

isPositive:: Int -> Bool

isPositive x 
        | x > 0 = True
        | otherwise = False
        
-- isEven function: returns a bool showing whether the input parameter is even or not
        
isEven:: Int -> Bool

isEven x
        | even x = True
        | otherwise = False
        
        
-- myRange function takes 2 integers parameters and returns a list of all integers numbers between them

myRange:: Int -> Int -> [Int]

myRange x y
        | x > y = error "Lower is greater than high"
        | x == y = [x]
        | x < y = [x] ++ myRange (x + 1) y
        
        
        
-- myRange Infinite List
-- This is an infinite list: if you give it the computation will never end.
-- It makes a lot of sense defining such functions because Haskell it's Call-By-Need (a.k.a. Lazy Evaluation)
-- Call this function with the take n function to take only the very firsts n elements of the possibly infinite ones

myInfiniteRange :: Int -> [Int]
myInfiniteRange x = [x] ++ myInfiniteRange (x+1)


-- take function: takes an integer n and an infinite list, and returns the first n elements of the list


mytake:: Int -> [a] -> [a]

mytake 0 _ = []
mytake _ [] = []
mytake n (x : xs) = x : mytake (n-1) xs

-- Infinite List example: [1,2 ..]

-- Infinite list of ones:

ones = 1 : ones

-- Let's define some types!!! Types in Haskell are really important
-- Bool is a type constructor, false and true are data constructors, in this case they are both nullary
--data Bool = False | True

data Point a = Point a a 


--Recursive Types: Tree type

data Tree a = Empty | Leaf a | Branch (Tree a) (Tree a)



-- An Example of a Tree:
-- aTree = Branch (Leaf 'a') (Branch (Leaf 'b') (Leaf 'c'))

-- aTree is type Tree Char



-- let's define a function for trees:

fringe:: Tree a -> [a]

fringe (Leaf x) = [x]
fringe (Branch left right) = fringe left ++ fringe right

-- First Class functions!
-- Haskell Map:

myMap:: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) =         (f x) : myMap f xs

--myMap (\x -> 1 + x) [1,2,3]

--TODO: Make the myMap partial computation using the currying thing

-- Myfilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
        | f x = x: myFilter f xs
        | otherwise = myFilter f xs
        
--  If f x evaluates true, i keep that value, otherwise I don't consider it
-- myFilter isEven [1,2,3]

-- FoldL
-- What about the accumulator in  fold? it's the second parameter!! 
-- fold takes as parameters a function that does something between a and a and returns an a, an initial value a and a list. It returns an a
-- remember: f is a 2 input function
myFoldl:: (a -> a -> a) -> a -> [a] -> a
myFoldl _ a [] = a
myFoldl f a (x : xs) = myFoldl f (f a x) xs


-- myFoldl (\x y -> x + y) 0 [1,2,3]

-- Zip takes 2 lists and returns a list of pairs contatining the elements of both lists in order


myZip :: [a] -> [b] -> [(a,b)]
myZip l [] = []
myZip [] l = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)


-- myZip [1,2,3] ['a','s','c']



-- Fibonacci number function: tells you how many fibonacci numbers there exists lesser than the given input


myFib:: Int -> Int

myFib 0 = 0
myFib 1 = 1
myFib 2 = 1
myFib n = myFib (n - 2) + myFib (n - 1)

--Remark: In this case the whole computation can be seen as a tree. 

-- factorial function

myFactorial :: Integer -> Integer
myFactorial n = product [1..n]

--Factorial implementation with foldl

myMapFactorial :: Integer -> Integer
myMapFactorial n = myFoldl (\x y -> x * y) 1 [1..n]



-- Implement it as a function actually returning the fibonacci numbers. This can be easily done with list comprehension:

fib = 1 : 1 : [a + b | (a,b) <- zip fib (tail fib)]
--It's the zip of the last number of fibonacci considered so far and the cdr of fib

--TODO Review and understand deeply
--zip [1,1,2,3] [1,2,3] = [(1,1),(1,2),(2,3)]

-- 1 : 1 : [zip [1,1] [1])]
-- 1: 1 : [(1,1)]
-- 1 : 1 : 2


-- but in reality fib isn't [1,1]... but how does it improve at each recursive call?

-- List Comprehension excercise: Right Triangles list

-- Remark: Do you see it?! It's a nullary function, simply returning an infinite list
rightTriangles:: [(Integer, Integer, Integer)]

rightTriangles = [(a,b,c) | c <- [1,2..], b <-[1..c], a <- [1..b], a^2 + b^2 == c^2]



-- let <bindings> in <expression>


-- Let's reimplement the quicksort
--takes a list of a and reorder it. a needs to be an instance of Ord class (so such that the <= and > operators are defined: it's possible to define a global ordering)
quicksort:: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs)= let right = quicksort [a | a <- xs, a <= x]
                        left = quicksort [a | a<- xs, a > x]
                        in right ++ [x] ++ left



--Implementing a type class: TrafficLights

data TrafficLight = Red | Yellow | Green


-- we make TrafficLight an instance of Eq
instance Eq TrafficLight where -- And now we define equality for TrafficLights
        Red == Red = True
        Yellow == Yellow = True
        Green == Green = True
        _ == _ = False

instance Show TrafficLight where
        show Red = "Red Light"
        show Yellow = "Yellow Light"
        show Green = "Green Light"


-- You aren't forced to define every ne type you define as an instance of classes: Haskell is smart enough to do it automatically with the 'deriving' keyword


data RPS = Rock | Paper | Scissors deriving (Show, Eq)

-- Since the ordering isn't trivial in this case, we explicitly define it:


instance Ord RPS where
        x <= y | x == y = True
        Rock <= Paper = True
        Paper <= Scissors = True
        Scissors <= Rock = True
        _ <= _ = False

--Since RPS is now an instance of Ord it is possible to call the quicksort function over it: quicksort [Paper,Rock, Rock, Scissors]


































 

        
        
        
        
        
        
        
        
        
 


