-- pattern match params from stack = produce result onto stack
operator (x:y:ys) "*" = (x * y):ys
operator (x:y:ys) "+" = (x + y):ys
operator (x:y:ys) "-" = (y - x):ys
operator (x:y:ys) "/" = (y / x):ys
operator (x:y:ys) "^" = (y ** x):ys
operator xs "sum" = [sum xs]
operator (x:xs) "ln" = log x:xs
operator xs numStr = read numStr:xs -- Read numerical token into stack

solveRPN :: String -> Float
solveRPN = head . foldl operator [] . words -- function forms by partial applications