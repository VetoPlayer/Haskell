-- let's define a lambda which can take decisions.

mylambda:: Int -> String
mylambda x = \x -> (case () of 
                _ | x > 0 -> "Positive"
                  | x == 0 -> "Zero"
                  | x < 0 -> "Negative")
