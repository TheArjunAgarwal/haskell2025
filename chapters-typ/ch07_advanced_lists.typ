#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise

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
map _ [] = []
map f (x:xs) = (f x) : (map f xs)

-- and much more clearly and concisely as
map f ls = [f l | l <- ls]
```
Similerly, we had seen `filter :: (a -> Bool) -> [a] -> [a] ` which used to take a boolean function, some predicate to satisfy, and return the list of elements satisfying this predicate. We can define this as:
```
filter _ [] = []
filter p (x:xs) = let rest = p xs in 
  if p x then x : rest else rest

-- and much more cleanly as
filter p ls = [l | l <- ls, p l]
```
 Another operation we can consider, though not explictly defined in Haskell, is cartisian product. Hopefully, you can see where we are going with this right?
 ```
 cart :: [a] -> [b] -> [(a,b)]
 cart xs ys = [(x,y) | x <- xs, y <- ys]
 ```
 Trying to define this reccursivly is much more cumbersome.
 ```
 cart [] _ = []
 cart (x:xs) ys = (go x ys) ++ (cart xs ys) where
  go _ [] = []
  go l (m:ms) = (l,m) : (go l ms)
 ```

Finally, let's talk a bit more about our pythogorean triplets example at the start of this section.
```
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
<& n ceil(log(n)) + 2n + 1/2log(n)\
$

Two things to note are that the above computation was very cumbersome. We will later see a way to make it a bit less cumbersome, at the cost of some information.

The second, for sufficiently large $n$, $n log(n)$ dominates the equation. That is $
exists m op(s.t.) forall n > m : n log(n) > 2n > 1/2log(n)
$
This means that as $n$ becomes large, we can sort of ignore the other terms. We will later prove, that given no more information other than the fact that the shape of the elemeents in the list is such that they can be compared, we can't do much better. The dominating term, in the number of comparisins, will be $n log(n)$ times some constant. This later refers to chapter 10.

In practice, we waste some ammount of operations dividing the list in 2. What if we take our chances and approximatly divide the list into two parts?

This is the idea of quick sort. If we take a random element in the list, we expect half the elements to be lesser than it and half to be greater. We can use this fact to define quickSort by splitting the list on the basis of the first element and keep going. This can be implemented as:
```
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

Using the linearity of expectation(remember $EE(sum X) = sum EE(x)$?), we can say $EE(C(n)) = sum_(i, j) EE(X_(i,j)) = sum_(i,j) p_(i,j)$.

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
A simple thing we might want to do at times is to join two lists together as pairs. 

// cite
// citation 1
// citation 2
