import Prelude(Int,undefined)

(+) :: Int -> Int -> Int
(+) 1 0 = 1
(+) 1 1 = 2
(+) 2 1 = 3
(+) 3 2 = 5

(-) :: Int -> Int -> Int
(-) 2 1 = 1
(-) 2 2 = 0
(-) 3 1 = 2
(-) 3 2 = 1
(-) 4 1 = 3
(-) 4 2 = 2

fib :: Int -> Int
fib 0 = 0
fib 1 = 1 
fib n = (+) (fib ((-) n 1)) (fib ((-) n 2))

belphegorsExpression :: Int
belphegorsExpression = fib 4