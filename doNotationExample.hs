mytryfunction = Just 3 >>= (\x ->
                Just "!" >>= (\y ->
                Just (show x ++ y)))
                
dofunction = do
          x <- Just 3
          y <- Just "!"
          Just (show x ++ y)
          
