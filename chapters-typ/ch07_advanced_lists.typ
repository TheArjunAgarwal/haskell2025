#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Tree.typ" : tree
#import "../Modules/Tree.typ" : far_away

#let definition = def
#let example = it => [For example - \ #it]

= advanced lists (feel free to change it)

== List Comprehensions
As we have talked about before, Haskell tries to make it's syntax look as similer as possible to math notation. This is reprasented in one of the most powerful syntactic sugers in Haskell, list comprehension.

If we want to talk about all pythogorean triplets using integers from $1-n$, we could express it mathematically as 
$
  {(x,y,z) | x,y,z in {1,2,dots,n}, x^2 + y^2 = z^2}
$
which can be written in Haskell as
```
[(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]
```
This allows us to define a lof of operations we have seen before, in ch 1, in rather concise manner.

For example, `map :: (a-> b) -> [a] -> [b]` which used to apply a function to a list of elements of a suitable input type and gave a list of the suitable output type. Basically, `map f [a1,a2,a3] = [f a1, f a2, f a3]`. We can define this in two ways:
```
-- | Defining map using pattern matching and list comprehension
map _ [] = []
map f (x:xs) = (f x) : (map f xs)

-- and much more clearly and concisely as
map f ls = [f l | l <- ls]
```
Similerly, we had seen `filter :: (a -> Bool) -> [a] -> [a] ` which used to take a boolean function, some predicate to satisfy, and return the list of elements satisfying this predicate. We can define this as:
```
-- | Defining filter using pattern matching and list comprehension
filter _ [] = []
filter p (x:xs) = let rest = p xs in 
  if p x then x : rest else rest

-- and much more cleanly as
filter p ls = [l | l <- ls, p l]
```
 Another operation we can consider, though not explictly defined in Haskell, is cartisian product. Hopefully, you can see where we are going with this right?
 ```
-- | Defining cartisian product using pattern matching and list comprehension
 cart :: [a] -> [b] -> [(a,b)]
 cart xs ys = [(x,y) | x <- xs, y <- ys]
 
 -- Trying to define this reccursivly is much more cumbersome.
 
cart [] _ = []
cart (x:xs) ys = (go x ys) ++ (cart xs ys) where
  go _ [] = []
  go l (m:ms) = (l,m) : (go l ms)
```

Finally, let's talk a bit more about our pythogorean triplets example at the start of this section.
```
-- | A naive way to get pythogorean triplets
pythNaive :: Int -> [(Int, Int, Int)]
pythNaive n = [(x,y,z) | 
          x <- [1..n], 
          y <- [1..n], 
          z <- [1..n], 
          x^2 + y^2 == z^2]
```
For `n = 1000`, we get the answer is some 13 minutes, which makes sense as our code is basically considering the $1000^3$ triplets and then culling the ones which are not pythogorean. But could we do better?

A simple idea would be to not check for `z` as it is implied by the choice of `x` and `y` and instead set the condition as
```
-- | A mid way to get pythogorean triplets
pythMid n = [(x, y, z) |
    x <- [1..n],
    y <- [1..n],
    let z2 = x^2 + y^2,
    let z = floor (sqrt (fromIntegral z2)),
    z * z == z2]
```
This is clearly better as we will be only considering some $1000^2$ triplets.
Continuing with our example, for `n = 1000`, we finish in 1.32 seconds. As we expected, that is already much, much better than the previous case.

Also notice that we can define variables inside the comprehension by using the `let` syntax.

 However, there is one final optimization we can do. The idea is that $x > y$ or $ x < y$ for pythogorean triplets as $sqrt 2$ is irrational. So if we can somehow, only evaluate only the cases where $x < y$ and then just genrate $(x,y,z)$ and $(y,x,z)$; we almost half the number of cases we check. This means, our final optimized code would look like:
```
-- | The optimal way to get pythogorean triplets
pythOpt n = [t|
    x <- [1..n],
    y <- [(x+1)..n],
    let z2 = x^2 + y^2,
    let z = floor (sqrt (fromIntegral z2)),
    z * z == z2,
    t <- [(x,y,z), (y,x,z)]
    ]
```
This should only make some $(1000 * 999)/2$ triplets and cull the list from there. This makes it about twice as fast, which we can see as for $n=1000$, we finish in $0.68$ seconds.

Notice, we can't return two things in a list comprehension. That is, `pythOpt n = [(x,y,z), (y,x,z) | <blah blah>]` will given an error. Intead, we have to use `pythOpt n = [ t | <blah blah>, t <- [(x,y,z), (y,x,z)]]`.

Another intresting thing we can do using list comprehension is sorting. While further sorting methods and their speed is discussed in chapter 10, we will focus on a two methods of sorting: Merge Sort and Quick Sort.

We have seen the idea of divide and conquor before. If we can divide the problem in smaller parts and combine them, without wasting too much time in the spliting or combining, we can solve the problem. Both these methods work on this idea.

Merge Sort divides the list in two parts, sorts them and then merges these sorted lists by comparing element to elemeent.
We can do this recursion with peace of mind as once we reach 1 element lists, we just say they are sorted. That is `mergeSort [x] = [x]`.

Just to illustrate, the merging would work as follows: `merge [1,2,6] [3,4,5]` would take the smaller of the two heads till both lists are empty. This works as as both the lists are sorted. The complete evaluation is something like:
```
merge [1,2,6] [3,4,5]
= 1 : merge [2,6] [3,4,5]
= 1 : 2 : merge [6] [3,4,5]
= 1 : 2 : 3 : merge [6] [4,5]
= 1 : 2 : 3 : 4 : merge [6] [5]
= 1 : 2 : 3 : 4 : 5 : merge [6] []
= 1 : 2 : 3 : 4 : 5 : 6 : merge [] []
= 1 : 2 : 3 : 4 : 5 : 6 : []
= [1,2,3,4,5,6]
```
So we can implement `merge`, rather simply as
```
-- | The merge function of mergesort
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y 
  then x : merge xs (y:ys) 
  else y : merge (x:xs) ys
```
Note, we can only sort a list which has some definition of order on the elements. That is the elements must be of the typeclass `Ord`.

To implement merge sort, we now only need a way to split the list in half. This is rather easy, we have already seen `drop` and `take`. An inbuilt function in Haskell is `splitAt :: Int -> [a] -> ([a], [a])` which is basically equivalent to `splitAt n xs = (take n xs, drop n xs) `.

That means, we can now merge sort using the function
```
-- | An implementation of mergesort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right) where
  (left,right) = splitAt (length xs `div` 2) xs
```

#exercise(sub : "MergeSort Works?")[
  Prove that merge sort indeed works. A road map is given

  (i) Prove that `merge` defined by taking the smaller of the heads of the lists reccursivly, produces a sorted list given the two input lists were sorted. The idea is that the first element choosen has to be the smallest. Use induction of the sum of lengths of the lists.

  (ii) Prove that `mergeSort` works using induction on the size of list to be sorted.
]

This is also a very efficent way to sort a list. If we define a function $C$ that count the number of comparisions we make, 
$C(n) < 2*C(ceil(n/2)) + n$ where the $n$ comes from the merge.

This implies $
C(n) <& n ceil(log(n)) C(1) + n + ceil(n/2) + ceil(ceil(n/2)/2) + dots + 1 \
<& n ceil(log(n)) + n + (n+1)/2 + ceil((n+1)/4) + dots +1\
=& n ceil(log(n)) + n + n/2 + 1/2 + n/4 + 1/2 + dots +1\
<& n ceil(log(n)) + 2n + 1/2 ceil(log(n))\
<& n (log(n) + 1) + 2n + 1/2 (log(n) + 1)\
=& n log(n) + 3n + 1/2 log(n) + 1/2
$

Two things to note are that the above computation was a bit cumbersome. We will later see a way to make it a bit less cumbersome, albeit at the cost of some information.

The second, for sufficiently large $n$, $n log(n)$ dominates the equation. That is $
exists m op(s.t.) forall n > m : n log(n) > 3n > 1/2log(n) > 1/2
$
This means that as $n$ becomes large, we can sort of ignore the other terms. We will later prove, that given no more information other than the fact that the shape of the elemeents in the list is such that they can be compared, we can't do much better. The dominating term, in the number of comparisins, will be $n log(n)$ times some constant. This later refers to chapter 10.

In practice, we waste some ammount of operations dividing the list in 2. What if we take our chances and approximatly divide the list into two parts?

This is the idea of quick sort. If we take a random element in the list, we expect half the elements to be lesser than it and half to be greater. We can use this fact to define quickSort by splitting the list on the basis of the first element and keep going. This can be implemented as:
```
-- | An implementation of Quick Sort
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort [l | l <- xs, l > x] ++ [x] ++ quickSort [r | r <- xs, r <= x]
``` 

#exercise(sub : "Quick Sort works?")[
  Prove that Quick Sort does indeed works. The simplest way to do this is by induction on length.
]

Clearly, With $n$ being the length of list, $C(n)$ is a random variable dependent on the permutation of the list. 

Let $l$ be the number of elements less than the first elements and $r = n-l-1$. This means $C(n) = C(l) + C(r) + 2(n - 1)$ where the $n-1$ comes from the list comprehension.

In the worst case scenario, our algoritm could keep spliting the list into a length $0$ and a length $n-1$ list. This would screw us very badly.

As $C(n) = C(0) + C(n-1) + 2(n - 1)$ where the $n-1$ comes from the list comprehension and the $(n-1)+1$ from the concatination.
Using $C(0) = 0$ as we don't make any comparisions, This evaluates to
$
  C(n) &= C(n-1) + 2(n-1)\
  &= 2(n-1) + 2(n-2) + dots + 2\
  &= 2 * (n(n-1))/2\
  &= n^2 - n
$
Which is quite bad as it grows quadratically. Furthermore, the above case is also common enough. How common? 
#exercise(sub : "A Strange Proof")[
Prove $2^(n-1) <= n!$
]

Then why are we intrested in Quick Sort? and why is named quick?

Let's look at the average or expected number of comparision we would need to make!

Consider the list we are sorting a permutation of $[x_1, x_2, dots , x_n]$. Let $X_(i,j)$ be a random variable which is $1$ if the $x_i$ and $x_j$ are compared and $0$ otherwise. Let $p_(i,j)$ be the 
probability that $x_i$ and $x_j$ are compared. Then, $EE(X_(i,j)) = 1 * p + 0 * (1-p) = p$.

Using the linearity of expectation (remember $EE(sum X) = sum EE(x)$?), we can say $EE(C(n)) = sum_(i, j) EE(X_(i,j)) = sum_(i,j) p_(i,j)$.

Using the same idea we used to reduce the number of pythogoream triplets we need to check, we rewrite this summation as
$
  EE(C(n)) &= sum_(i,j) p_(i,j)\
  &= sum_(i=1)^n sum_(j=i+1)^n p_(i,j)\
$
Despite a toothy appearence, this is rather easy and elegent way to actually compute $p_(i,j)$.\

Notice that each element in the array (except the pivot) is compared only to the pivot at each level of the recurrence. To compute $p_(i,j)$, 
we shift our focus to the elemenents $[x_i, x_(i+1), dots, x_j]$. If this is split into two parts, $x_i$ and $x_j$ can no longer be compared.
Hence, $x_i$ and $x_j$ are compared only when from the first pivot from the range $[x_i, x_(i+1), dots, x_j]$ is either $x_i$ or $x_j$.

This clearly has probability $p_(i,j) = 1/(j-i+1) + 1/(j-i+1) = 2/(j-i+1)$. Thus,
$
  EE(C(n)) &= sum_(i=1)^n sum_(j=i+1)^n 2/(j-i+1)\
  &= sum_(i=1)^n 2 (1/2 + dots + 1/(n-i+1))\
  &= 2 sum_(i=1)^n (1 + 1/2 + dots + 1/(n-i+1) - 1)\
  &<= 2 sum_(i=1)^n log(i)\
  &<= 2 sum_(i=1)^n log(n)\
  &<= 2 n log(n)
$
Considering the number of cases where the comparisons with $n^2 - n$ operations is $2^(n-1)$, 
Quick Sort's expected number of operations is still less than $2 n log(n)$ which, as we discussed, is optimal.

This implies that there are some lists where Quick Sort is extreamly efficent and as one might expect there are many such lists. 
This is why languages which can keep states (C++, C, Rust etc) etc use something called Introsort which uses 
Quick Sort till the depth of recusion reaches $log(n)$ (at which point it is safe to say we are in one of the not nice cases); 
then we fallback to Merge Sort or a Heap/Tree Sort(which we will see in chapter 11).

Haskell has an inbuilt `sort` function you can use by putting `import Data.List` at the top of your code. 
This used to use quickSort as the default but in 2002, 
Ian Lynagh changed it to Merge Sort. This was motivated by the fact that 
Merge Sort gurentees sorting in $n log(n) + dots$ comparisons while Quick Sort will sometimes finish much quicker (pun not intended) 
and other times, just suffer.

As a dinal remark, our implementation of the Quick Sort is not the most optimal as we go through the list twice, 
but it is the most aesthetically pleasing and concise.
#exercise(sub : "Faster Quick Sort")[
  A slight improvment can be made to the implementation by not using list comprehension and instead using a helper function, to traverse the list only once.

  Try to figure out this implementation.
]

== Zip it up!
Have you ever suffered through a conversation with a very dry person with the goal of getting the contact information of a person you are actually intrested in? If you haven't well, that is what you will have to do now.

#exercise(sub : "The boring zip")[
  Haskell has an inbuilt function called `zip`. It's behaviour is as follows
  ```
  >>> zip [1,2,3] [4,5,6]
  [(1,4),(2,5),(3,6)]
  >>> zip [1,2,3] [4,5,6,7]
  [(1,4),(2,5),(3,6)]
  >>> zip [0,1,2,3] [4,5,6]
  [(0,4),(1,5),(2,6)]
  >>> zip [0,1,2,3] [True, False, True, False]
  [(0,True),(1,False),(2,True),(3,False)]
  >>> zip [True, False, True, False] "abcd"
  [(True,'a'),(False,'b'),(True,'c'),(False,'d')]
  >>> zip [1,3..] [2,4..]
  [(1,2),(3,4),(5,6),(7,8),(9,10),(11,12),(13,14),(15,16),(17,18),(19,20)...]
  ```
  What is the type signature of `zip`? How would one implement `zip`?
]
The solution to the above exercise is, rather simply:
```
-- | Implementation of zip function
zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys
```

While one could think of some places where this is useful, all of the uses seem rather dry. But now that `zip` has opened up to us, we will ask them about `zipWith`. The function `zipWith` takes two lists, a binary function, and joins the lists using the function. The possible implementations are:
```
-- | Implementation of zipWith function
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = (x `f` y) : zipWith f xs ys
```

#exercise(sub : "Alternate definitions")[
  While we have defined `zip` and `zipWith` independently here, can you:
  (i) Define `zip` using `zipWith`?
  (ii) Define `zipWith` using `zip`?
]
Now one might feel there is nothing special about `zipWith` as well, but they would be wrong. First, it saves us form defining a lot of things: `zipWith (+) [0,2,5] [1,3,3] = [1,5,8]` is a common enough use. And then, it leads to a lot of absolutly mindblowing pieces of code.
```
-- | The zipWith fibonnaci
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
```
Belive it or not, this should output the fibonnaci sequence. The idea is that Haskell is lazy! This means lists are computed one element at a time, starting from the first. Tracing the computation of the elements of `fibs`:

1. Since by definition `fibs = 0: 1: (something)`, the first element is `0`.

2. This is again easy, since `fibs = 0 : 1 : (something)`, so the second element is `1`.

3. This is going to be the first element of `something`, i.e. the part that comes after the `0 : 1 :`. So, we need to compute the first element of `zipWith (+) fibs (tail fibs)`. How do we do this? We compute the first element of `fibs` and the first element of `tail fibs` and add them. We know already, that the first element of `fibs` is `0`. And we also know that the first element of `tail fibs` is the second element of `fibs`, which is `1` So, the first element of `zipWith (+) fibs (tail fibs)` is `0 + 1 = 1`.

4.It is going to be the fourth element of `fibs` and the `second of zipWith (+) fibs (tail fibs)`. Again, we do this by taking the second elements of `fibs` and `tail fibs` and adding them together. We know that the second element of `fibs` is `1`. The second element of `tail fibs` is the third element of `fibs`. But we just computed the third element of `fibs`, so we know it is `1`. Adding them together we get that the fourth element of fibs is `1 + 1 = 2`.

This goes on and one to generate the fibonnaci sequence. To recall, the naive
```
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n-1) + fibNaive (n-2)
```
is much slower. This is becuase the  computation tree for say `fib 5` looks something like:

#tree(spread : 2.3, (
  [`fibNaive 5`],
  (
    [`fibNaive 4`],
    (
      [`fibNaive 3`],
      (
        [`fibNaive 2`],
        ([`fibNaive 1`]),
        ([`fibNaive 0`])
      ),
      ([`fibNaive 1`])
    ),
    (
      [`fibNaive 2`],
      ([`fibNaive 1`]),
      ([`fibNaive 0`])
    )
  ),
  (
    [`fibNaive 3`],
    (
      [`fibNaive 2`],
      ([`fibNaive 1`]),
      ([`fibNaive 0`])
    ),
    ([`fibNaive 1`])
  )
))

And one can easily see that we make a bunch of unneccesary recomputations, and thus a lot of unneccesary additions. On the other hand, using our `zipWith` method, only computes things once, and hence makes only as many additions as required.
#exercise[
  Try to trace the computation of `fib !! 5` and make a tree.
]
Let's now try using this trick to solve some harder problems.
#exercise(sub: "Tromino's Pizza I")[
Tromino's sells slices of pizza in only boxes of 3 pieces or boxes of 5 pieces. You can only buy a whole number of such boxes. Therefore it is impossible to buy exactly 2 pieces, or exactly 4 pieces, etc. Create list `possiblePizza` such that if we can buy exactly `n` slices, `possiblePizza !! n` is `True` and `False` otherwise.
]
The solution revolves around the fact $f(x) = f(x-3) or f(x-5)$. A naive implementation could be
```
possiblePizzaGo :: Int -> Bool
possiblePizzaGo x
  | x == 0    = True
  | x < 3     = False
  | x == 5    = True
  | otherwise = possiblePizzaGo (x - 3) || possiblePizzaGo (x - 5)

possiblePizza = [possiblePizzaGo x | x <- [0..]]
```
This is slow for the same reason as `fibNaive`. So what can we do? Well, use zipWith.

```
possiblePizza = True : False : False : True : False : zipWith (||) (possiblePizza) (drop 3 possiblePizza)
```
Note, we need to define till the 5th place as otherwise the code has no way to know we can do 5 slices.

#exercise(sub: "Tromino's Pizza II")[
  Tromino's has started to charge a box fees. So now given a number of slices, we want to know the minimum number of boxes we can achive the order in. Create a list `minBoxPizza` such that if we can buy exactly `n` slices, the list displays `Just` the minimum number of boxes the order can be achived in, and `Nothing` otherwise. The list is hence of type `[Maybe Int]`.

  Hint : Create a helper function to use with the `zipWith` expression.
]

One more intresting thing we can talk about is higher dimensional `zip` and `zipWith`. One way to talk about them is `zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]` and `zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]`.  These are defined exactly how might expect them to be.
```
zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 [] _ _ = []
zip3 _ [] _ = []
zip3 _ _ [] = []
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs


zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 _ [] _ _ = []
zipWith3 _ _ [] _ = []
zipWith3 _ _ _ [] = []
zipWith3 f (x:xs) (y:ys) (z:zs) = (f x y z) : zipWith3 f xs ys zs
```
#exercise[A slightly tiresome exercise, try to define `zip4` and `zipWith4` blind.]

Haskell predefines till `zip7` and `zipWith7`. We are yet to see anything beyond `zipWith3` used in code, so this is more than enough. Also, if you truly need it, `zip8` and `zipWith8` are not that hard to define.

#exercise(sub : "Tromino's Pizza III")[
  Tromino's has introduced a new box of size 7 slices. Now they sell $3,5,7$ slice boxes. They still charge the box fees. So now given a number of slices, we still want to know the minimum number of boxes we can achive the order in. Create a list `minBoxPizza` such that if we can buy exactly `n` slices, the list displays  `Just` the minimum number of boxes the order can be achived in, and `Nothing` otherwise. The list is hence of type `[Maybe Int]`.

]

Another idea of dimension would be something that could join together two grids, something with type signature `zip2d :: [[a]] -> [[b]] -> [[(a,b)]]` and `zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]`.
```
zip2d :: [[a]] -> [[b]] -> [[(a,b)]]
zip2d = map zip

zipWith2d :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipWith2d = zipWith . zipWith
```
The second definition should raise immidiete alarms. It seems too good to be true. Let's formally check
```
zipWith . zipWith $ (a -> b -> c) [[a]]  [[b]]
= zipWith (zipWith (a -> b -> c))  [[a]] [[b]] -- Using the fact that composition only allows one of the inputs to be pulled inside
= [
    zipWith (a -> b -> c) [a1] [b1],
    zipWith (a -> b -> c) [a2] [b2],
    ...
  ]
= [[c1], [c2], ...]
= [[c]]
```
This also implies `zip2d = zipWith.zipWith $ (\x y -> (x,y))` is also a correct definition. Also surprisingly, `zipWith . zipWith . zipWith` has the type signature `(a -> b -> c) -> [[[a]]] -> [[[b]]] -> [[[c]]]`. You can see where we are going with this...
#exercise(sub : "Composing zipWith's")[
  What should the type signature and behaviour of `zipWith . zipWith . <n times> . zipWith` be? Prove it.
]
#exercise(sub : "Unzip")[
Haskell has an inbuilt function called `unzip :: [(a,b)] -> ([a],[b])` which takes a list of pairs and provides a pair of list in the manner inverse of `zip`.

Try to figure out the implementation of `unzip`.
]

== Folding, Scaning and The Gate to True Powers
A lot of reccursion on lists has the following structure
```
g [] = v -- The vacous case
g (x:xs) = x `f` (g xs)
```
That is, the function `g :: [a] -> b` maps the empty list to a value `v`, of say type `b`, and for non-empty lists, the head of the list and the result of recursively processing the tail are combined using a function or operator `f :: a -> b -> b`.

Some commone examples from the inbuilt functions are:
```
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + (sum xs)

product :: [Int] -> Int
product [] = 1 -- The sturcuture forces this choice as other wise, the product of full lists may become incorrect.
product (x:xs) = x * (product xs)

or :: [Bool] -> Bool
or [] = False -- As the structure of our implementation forces this to be false, or otherwise, everything is true.
or (x:xs) = x || (or xs)

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && (and xs)
```

We will also see a few more examples in a while, but one can notice that this is a common enough pattern. So what do we do? We abstract it.

```
-- | Definition of foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ v [] = v
foldr f v (x:xs) = x `f` (foldr f v xs)
```

This shortens our definitons to
```
sum = foldr (+) 0
product = foldr (*) 1
or = foldr (||) False
and = foldr (&&) True
```

Sometimes, we don't wish to define a base case or maybe the logic makes it so that doing so is not possible, then you use `foldr1 :: (a -> b -> b) -> [a] -> a` defined as
```
-- | Definition of foldr1
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [x] = x
foldr1 f (x:xs) = x `f` (foldr f xs)
```

Like we could now define `product = foldr1 (*)` which is much more clean as we don't have to define a weird vacous case.

Let's now discuss the naming of the pattern. Recall, `[1,2,3,4]` is syntactic suger for `1 : 2 : 3 : 4 : []`. We are just allowed to write the former as it is more aesthetic and convinient. One could immidietly see that 
```
foldr v f [1,2,3,4] = 1 `f` (2 `f` (3 `f` (4 `f` v)))
-- and if f is right associative
= 1 `f` 2 `f` 3 `f` 4 `f` v
```
We have basically changed the cons (`:`) into the function and the empyy list (`[]`) into `v`. But notice the brackets, the evaluation is going from right to left.

Using trees, A list can be reprasented in the form

#tree(
(`(:)`,
`x1`,
(`(:)`,
`x2`,
(`(:)`,
`x3`,
(far_away(`(:)`),
`xn-1`,
(`(:)`,
`xn`,
`[]`
)
)
)
)
)
)

which is converted to:

#tree(
(`f`,
`x1`,
(`f`,
`x2`,
(`f`,
`x3`,
(far_away(`f`),
`xn-1`,
(`f`,
`xn`,
`v`
)
)
)
)
)
)

However, what if our function is left associative? After all, if this was the only option, we would have called it `fold`, not `foldr` right?

The recursive pattern
```
g :: (b -> a -> b) -> b -> [a] -> b
g _ v [] = v
g f v (x:xs) = g (f v x) xs
```
is abstracted to `foldl` and `foldl1` respectively.
```
-- | Definition of foldl and foldl1
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ v [] = v
foldl f v (x:xs) = foldl (f v x) xs

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs 
```
And as the functions we saw were commutative, we could define them as
```
sum       = foldl (+) 0
product   = foldl (*) 1
or        = foldl (||) False
and       = foldl (&&) True
```
There is one another pair of function defined in the fold family called `foldl'` and `foldl1'` which are faster than `foldl` and `foldl1` and don't require a lot of working memory. This makes them the defualts used in most production code, but to understand them well, we need to discuss how haskell's lazy computation actually works and is there a way to bypass it. This is done in chapter 9. We will use `foldl` and `foldl1` till then.

The computation of `foldl` proceeds like
```
foldl v f [1,2,3,4] = ((((v `f` 1) `f` 2) `f` 3) `f` 4)
--  and if f is left associative
= v `f` 1 `f` 2 `f` 3 `f` 4
```
Or in tree form as

#tree(
(`(:)`,
`x1`,
(`(:)`,
`x2`,
(`(:)`,
`x3`,
(far_away(`(:)`),
`xn-1`,
(`(:)`,
`xn`,
`[]`
)
)
)
)
)
)

which is converted to:

#tree(
(`f`,
  (`f`,
    (`f`,
    (far_away(`f`),
      (`f`,`v`,`x1`),`x2`
    ),`xn-2`
    ),`xn-1`
  ), `xn`
)
)

Another very cute picture to summerize the diffrences is:
#image("../images/image.png")

Similer to how `unzip` was for `zip`, could we define `unfoldr`, something that takes a generator function and a seed value and genrates a list out of it.

What could the type of such a function be? Well, like with every design problem; let's see what our requirements are:
- The list should not just be one element over and over. Thus, we need to be able to update the seed after every unfolding.
- There should be a way for the list to terminate.

So what could the type be? We can say that our function must spit out pairs: of the new seed value and the element to add to the list. But what about the second condition?

Well, what if we can spit out some seed which can never come otherwise and use that to signal it? The issue is, that would mean the definition of the function has to change from type to type.

Instead, can we use something we studied in ch-6? Maybe. #footnote("Pun intended.")
```
-- | Implementation of unfoldr
unfoldr :: (a -> Maybe (a,b)) -> a -> [b]
unfoldr gen seed = go seed
  where
    go seed = case gen seed of
        Just (x, newSeed) -> x : go newSeed
        Nothing         -> []
```
For example, we could now define some library functions as:
```
replicate :: Int -> a -> [a]
-- replicate's an value n times
replicate n x = unfoldr gen n x where
    rep 0 = Nothing
    rep m = Just (x, m - 1)

itterate :: (a -> a) -> a -> [a]
-- given a function f and some starting value x
-- outputs the infinite list [x, f x, f f x, ...]
itterate f seed = unfold (\x -> Just (x, f x)) seed
```
While `foldr` and `foldl` are some of the most common favorite function of haskell proggramers;  `unfoldr` remains mostly ignored. It is so ignored that to get the inbuilt version, one has to`import Data.List`. We will soon see an eggregious case where Haskell's own website ignored it. One of the paper we reffered was litrally titled "The Under-Appreciated Unfold".

#exercise(sub : "Some more inbuilt functions")[
  Implement the following functions using fold and unfold.

  (i) `concat :: [[a]] -> [a]` concats a list of lists into a single list. For example: `concat [[1,2,3],[4,5,6],[7,8],[],[10]] = [1,2,3,4,5,6,7,8,9, 10]`

  (ii) `cycle :: [a] -> [a]` cycles through the list endlessly. For example: `cycle [2, 3, 6, 18] = [2, 3, 6, 18, 2, 3, ...]`

  (iii) `filter :: (a -> Bool) -> [a] -> [a]` takes a predicate and a list and filters out the elements satisfying that predicate.

  (iv) `concatMap :: (a -> [b]) -> [a] -> [b]` maps a function over all the elements of a list and concatenate the resulting lists. Do not use `map` in your definition.

  (v) `length :: [a] -> Int` gives the number of elements in the provided list. Use `foldr` or `foldl`.

  (vi) `iterate :: (a -> a) -> a -> [a]` takes a function `f` and element `a` and returns `[a, f a, f (f a), ...]` (an infinite list). 
]

#exercise(sub : "Base Conversion")[
  (i) Comvert list of digits in base `k` to a number. That is `lis2num :: Int -> [Int] -> Int` with the usage `lis2num base [digits]`.

  (ii) Given a number in base 10, convert to a list of digits in base `k`. `num2lis :: Int -> Int -> [Int]` with the usage `num2list base numberInBase10`
]
Let's go part by part. The idea of the first question is simply to understand that `[4,2,3]` in base `k` reprasents $4 * k^2 + 2 * k + 3 * k^0 = ((0 * k + 4) * k + 2)*k + 3$; doesn't this smell like `foldl`?

```
lis2num :: [Int] -> Int -> Int
lis2num k = foldl (\x y -> k * x + y) 0
```

For part two, the idea is that we can base convert using repeated division. That is,
```
423 `divMod` 10 = (42, 3)
42 `divMod` 10 = (4, 2)
4 `divMod` 10 = (0, 4)
```
It is clear that we terminate when the quotient reaces $0$ and then just take the remainders. Does this sound like `unfoldr`?
```
num2lis :: Int -> Int -> [Int]
num2lis k = reverse . unfoldr gen where
  gen 0 = Nothing
  gen x = Just $ (x `mod` k, x `div` k)
```

#exercise(sub : "A list of Primes")[
  This is the time when Haskell itself forgot that the `unfoldr` function exists. The website offers the following method to make a list of primes in Haskell as an advertisment for the language.
  ```
  primes = filterPrime [2..] where
  filterPrime (p:xs) =
    p : filterPrime [x | x <- xs, x `mod` p /= 0]
  ```
  Understand this code (write a para explaining exactly what is happening!) and try to define a shorter (and more aesthetic) version using `unfoldr`.
]
The answer is litrally doing what one would do on paper. Like describing it would be a disservice to the code.
```
-- | list of primes using unfoldr
sieve (x:xs) = Just (x, filter (\y -> y `mod` x /= 0) xs)
primes = unfoldr sieve [2..]
```
#exercise(sub : "Subsequences")[
  Write a function `subslists :: [a] -> [[a]]` which takes a list and returns a list of sublists of the given list. For example: `sublists "abc" = "","a","b","ab","c","ac","bc","abc"` and `sublists [24, 24] = [[],[24],[24],[24,24]]`. 

  Try to use the fact that a sublist either contains an element or not. Second, the fact that sublists correspond nicely to binery numerals may also help.

  You function must be compatable with infinite lists, that is `take 10 $ sublists [1..] = [[], [1], [2], [1,2], [3], [1,3], [2,3], [1,2,3], [4], [1,4]]` should work.
]
*Please fill in the blanks below*

A naive, non-infinite compatable definiton is:
```
sublists [] = ______
subslists (x:xs) = concatMap (\ys -> ______) (sublists xs)
```

On an infinite list, this definition gets stuck in an non-productive loop because we must traverse the entire list before it returns anything.

Note that on finite cases, the first sublist returned is always `______`. This means we can state this as `sublists xs == _______ : ______ (sublists xs)`. It is sensible to extend this equality to the infinite case, due to the analogy of `________`. 

By making this substitution, we produce the definition that can handle infinite lists, from which we can calculate a definition that’s more aesthetically pleasing and slightly more efficient:
```
____________ -- Base case
sublists (x:xs)  = _____ : ______ : concatMap (\ys -> ______) (tail . sublists xs)
```

We can clean this definition up by calculating definitions for `tail.sublists x` and renaming it something like `nonEmpties`. We start by applying tail to both sides of the two cases. `nonEmpties [] = tail.sublists [] = _______` and `nonEmpties (x:xs) = tail.sublists (x:xs) = _______`

Substituting all thins through the definition.
```
-- | Space to write the definition of sublists





```

This function can be called in Haskell through the `subsequences` function one gets on importing `Data.Lists`. Our definition is the most efficient and is what is used internally.

Finally, a question which would require you to use a lot of functions we just defined:
#exercise(sub: "The Recap Problem (Euler's Project 268)")[
It can be verified that there are $23$ positive integers less than $1000$ that are divisible by at least four distinct primes less than $100$.

Find how many positive integers less than $10^16$ are divisible by at least four distinct primes less than $100$.

Hint : Thik about PIE but not $pi$.
]


=== Numerical Integration
To quickly revise all the things we just learnt, we will try to write our first big-boy code.

Let's talk about numerical Integration. Numerical Integration refers to finding the value of integral of a function, given the limits. This is also a part of the mathematical computing we first studied in chapter 3. To get going, a very naive idea would be:
```
easyIntegrate :: (Float -> Float) -> Float -> Float -> Float
easyIntegrate f a b = (f b + f a) * (b-a) / 2
```

This is quite inaccurate unless `a` and `b` are close. We can be better by simply dividing the integral in two parts, ie $integral_a^b f(x) diff x = integral_a^m f(x) diff x + integral_m^b f(x) diff x$ where $a < m < b$ and approximate these parts. Given the error term is smaller in these parts than that of the full integral, we would be done. We can make a sequence converging to the integral we are intrested in as:
```
-- | Naive Integration
integrate :: (Float -> Float) -> Float -> Float -> [Float]
integrate f a b = (easyIntegrate f a b) : zipWith (+) (integrate f a m) (integrate f m b) where m = (a+b)/2 
```

If you are of the kind of person who likes to optimize, you can see a very simple inoptimality here. We are computing `f m` far too many times. Considering, `f` might be slow in itself, this seems like a bad idea. What do we do then? Well, ditch the aesthetic for speed and make the naive integrate as:

```
-- | Naive Integration without repeated computation
integrate f a b = go f a b (f a) (f b)

integ f a b fa fb = ((fa + fb) * (b-a)/2) : zipWith (+) (integ f a m fa fm) (integ f m b fm fb) where 
  m = (a + b)/2
  fm = f m
```

This process is unfortunatly rather slow to converge for a lot of fucntions. Let's call in some backup from math then. 

The elements of the sequence can be expressed as the correct answer plus some error term, ie $a_i = A + Epsilon$. This error term is roughly propotional to some power of the seperation between the limits evaluated (ie $(b-a), (b-a)/2 dots$) (the proof follows from Taylor exmapnsion of $f$. You are reccomended to prove the same). Thus,
$
  a_i = A + B times ((b-a)/2^i)^n\
  a_(i+1) = A + B times ((b-a)/2^(i+1))^n\
  => a_(i+1) - 1/2^n a_i = A(1-1/2^n) \
  => A = (2^n times a_(i+1) - a_i)/(2^n - 1)
$

This means we can improve our sequence by eliminating the error
```
elimerror :: Int -> [Float] -> [Float]
elimerror n (x:y:xs) = (2^^n * y - x) / (2^^n - 1) : elimerror n (y:xs)
```

However, we have now found a new problem. How in the world do we get `n`?

$
  a_i &= A + B times ((b-a)/2^i)^n\
  a_(i+1) &= A + B times ((b-a)/2^(i+1))^n\
  a_(i+2) &= A + B times ((b-a)/2^(i+2))^n\
  => a_i - a_(i+1) &= B times ((b-a)/2^i)^n times (1-1/2^n)\
  => a_(i+1) - a_(i+2) &= B times ((b-a)/2^i)^n times (1/2^n - 1/4^n)\
  => (a_i - a_(i+1)) / (a_(i+1) - a_(i+2)) &= (4^n - 2^n )/(2^n - 1) = (2^n (2^n-1))/(2^n - 1) = 2^n\
  => n &= log_2((a_i - a_(i+1))/(a_(i+1) - a_(i+2)) )
$

Thus, we can estimate `n` using the function `order`. We will be using the inbuilt function `round :: (RealFrac a, Integral b) -> a -> b` in doing so. In our case, `round :: Float -> Int`. 
```
order :: [Float] -> Int
order (x:y:z:xs) = round $ logBase 2 $ (x-y)/(y-z)
```

This allows us to improve our sequence 
```
improve :: [Float] -> [Float]
improve xs = elimerror (order xs) xs
```
One could make a very fast converging sequence as say `improve $ improve $ improve $ integrate f a b`.

But based on the underlying function, the number of `improve` may differ.

So what do we do?  We make an extreamly clever move to define a super sequence `super` as
```
super :: [Float] -> [Float]
super xs = map (!! 2) (iterate improve xs) -- remeber itterate from the excercises above?
```
I will re-instate, the implementation of `super` is extreamly clever. We are recursivly getting a sequence of more and more improved sequences of approximations and constructs a new sequence of approximations by taking the second term from each of the improved sequences. It turns out that the second one is the best one to take. It is more accurate than the first and doesn’t require any extra work to compute. Anything further, requires more computations to compute.

Finally, to complete our job, we define a function to choose the term upto some error.
```
within :: Float -> [Float] -> Float
within error (x:y:xs)
  | abs(x-y) < error = y
  | otherwise = within error (y:xs)
```

```
-- | An optimalized function for numerical integration
ans :: (Float -> Float) -> Float -> Float -> Float -> Float
ans f a b error = within error $ super $ integrate f a b
```

With this we are done!

== Time to Scan


// contine


// Include a numerical diffretiation part.











// cite
// citation 1
// citation 2
