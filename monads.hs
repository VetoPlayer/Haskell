import Control.Applicative
import Control.Monad

type Log = [String]

-- We define a Logger whose type is a tuple containing a and a Log, which is a List of Strings.
-- The type has as data accessor run
newtype Logger a = Logger { run :: (a, Log)}

-- let x = Logger (4, ["Ciaooo"])
-- run x --->> (4, ["Ciaooo"])

--Let's make Logger Showable,Eq, Functor, Applicative, Monads


instance (Show a) => Show (Logger a) where
        show (Logger a) = show a
        
instance (Eq a) => Eq (Logger a) where
        Logger (x,y) /= Logger (a,b) = (x /= a) || (y /= b)
       
instance Functor Logger where
        fmap = loggermap
        

-- Loggermap takes the elements of the logger, applies the given function f to the first element, and builds up a new Logger
loggermap:: (a -> b) -> Logger a -> Logger b      
loggermap f log =
                let (a,l) = run log
                    n = f a
                  in Logger (n, l)
                 

instance Applicative Logger where
        pure a = Logger (a, [])
        (<*>) = lapp
        
-- Logger applicative take a Logger containing a function, applies it to a Logger containing data. No Need to Concatenate.
lapp:: Logger (a -> b) -> Logger a -> Logger b
lapp lf lg =
        let (f, llf) = run lf-- Takes the function
            ln = loggermap f lg-- Map the function on the data
            (a, l) = run ln-- Extract that information
        in Logger (a, llf ++ l)-- Builds up a new logger having the (computed value, "New history of given values")
        
        
-- In our case (>>=) :: Logger a -> (a -> Logger b) -> Logger b         
        
instance Monad Logger where
        m >>= f =
         let (logdata, loghistory) = run m
             x = f logdata -- f can, from a broad point of view, do ANYTHING!!
             (logelaborateddata, newoperationhistory) = run x
         in Logger(logelaborateddata, loghistory ++ newoperationhistory)
              
-- OK we did it!!
-- Let's see now some applications: The interestin part of the whole Monad thing is the fact that the function (a -> Logger b) can do and be ANYTHING

-- Define a function that takes a number,add one and log the operation
-- Define a function that takes a number, Add ones, Builds up the Monadic Action

logAddOne:: (Num a) => a -> Logger b
logaddOne x = Logger (x+1, ["+1"])

-- Define a function taking a number, multiplying it for 2 and building up tha monadic action
logMultTwo:: (Num a) => a -> Logger b
logMultTwo x = Logger (x * 2, ["*2"])


-- OK!!! Now, since Logger is a Monad we can use >>=. For syntacti sugar, this is wrapped inside the do { <- } notation


-- Define a function that takes a logger, adds one, doubles the value and logs all the operations

logOps:: (Num a) => Logger a -> Logger a
logOps log = do
       v <- log -- Binds the logger value to v
       p1 <- logAddOne v
       m2 <- logMultTwo p1
       return m2 -- 


-- TODO See State Monadù

record:: String -> Logger ()
record s  = Logger ((), [s])









-- Let's do now some trees:



data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq)

singletonM :: (Show a) => a -> Logger (Tree a)
















                     
     
