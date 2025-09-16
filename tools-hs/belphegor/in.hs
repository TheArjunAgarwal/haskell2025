import Prelude(Int,undefined)

naturals = l 0 

l n = (:) n (l (succ n))

succ 0 = 1
succ 1 = 2
succ 2 = 3
succ 3 = 4 
succ 4 = 5
succ 5 = 6

belphegorsExpression = take 5 naturals

take 0 l = []
take n [] = []
take n ((:) x xs) = (:) x (take (pred n) xs)

pred 5 = 4
pred 4 = 3
pred 3 = 2
pred 2 = 1
pred 1 = 0