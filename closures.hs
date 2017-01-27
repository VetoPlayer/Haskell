-- Closures in Haskell are weak because it's  a pure language and you cannot modify the internal state
-- Haskell doesn't allow you to modify a variable later, or Ever

adder f x = (\ y -> x + y)
