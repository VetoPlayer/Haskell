--State Monad

-- Models stateful computation still letting Haskell to be a pure language.


-- Let's model an operating Stack

type Stack = [Int]

oldpop:: Stack -> (Int, Stack)
oldpop (x : xs)= (x, xs)

oldpush:: Int -> Stack -> ( () , Stack)
oldpush x stack = ( (), x : stack)

--Stack Manipulator:
stackManip:: Stack -> (Int, Stack)
stackManip stack = let
        ((), newStack1) = oldpush 3 stack
        (a, newStack2) = oldpop newStack1
        in oldpop newStack2
        
-- It would be a lot cooler if, instead of giving the new stack to each function, we just manage the state automatically.

-- The Control.Monad.State module provides us a newtype that wraps stateful computations


 
 
newtype MyState s a = MyState {runState:: s -> (a, s) }-- It EXPECT A LAMBDA!!!!!


-- OH MAI LAMBDA!!! DDDD:

-- The state monad wraps up a lambda function, called stateful computation.

-- A Stateful computation is a f taking a state and returning some value and a state: a -> (a,s)

-- For example: let x = MyState (\x -> (2,3))
-- (runState x) 5 evaluates (2,3)



-- This thing here actually makes more sense: 

-- let x = MyState (\x -> (x, x + 4))

-- Stateful computation manipulating a state of type s resulting in a type a

-- The Monad Typeclass wants a constructor with a single value like Maybe a
-- State s a has actually 2 values!! So we fix s as a part of the constructor

-- a is the result of the stateful computation.

-- You have a function f and a state st
instance Functor (MyState s) where
      fmap f st = let
                fstate = runState st -- Take what is contained in the State, which is a f , called Stateful Computation
                in MyState (\x -> let (a,b) = fstate x -- Feeds the stateful computation with THE CURRENT STATE!!!
                --you retrieve your tuple (a,b)
                                  in (f a, b)) -- Applies f to the result of the stateful computation. Then MyState constructor is then called.
                                  
                                  
-- To check out what is actually doing, type from the command line:

--let state = MyState (\x -> (2,x + 90))
--let result = fmap (+3) state
--(runState result) 4---> Evaluates (5,90)


instance Applicative (MyState s) where
        pure x = MyState $ \s -> (x,s) -- x is the value passed to the function, s the current state!!!!!
        stateFun <*> stateDat = let
                                fstatefunction = runState stateFun
                                fdatafunction = runState stateDat
                                in MyState (\x -> let
                                                (f, _) = fstatefunction x
                                                (a, s) = fdatafunction x
                                                in (f a, s))
-- To check out how this shit works:

-- let f = MyState (\x -> ((\y -> y + 4), 0))
-- let datastate = MyState (\x -> (10,100))
-- let result = f <*> datastate
-- (runState result) 0 ---> Evaluates (14,100)


instance Monad (MyState s) where
        return x = MyState $ \s -> (x,s)
        (MyState h) >>= f = MyState $ \s -> let
                                            (a, newState) = h s
                                            (MyState g) = f a
                                            in g newState



push :: Int -> MyState Stack ()
push a = MyState $ \s -> ((), a : s)


pop :: MyState Stack Int
pop = MyState $ \(s : xs) -> (s, xs)                      




newstackManip:: MyState Stack Int
newstackManip = do
        push 3
        a <- pop
        pop
        
        
ciao:: Int
ciao = 5        







































                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  

