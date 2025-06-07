#import "../Modules/style.typ": *
#show: codly-init.with()
#codly(languages: codly-languages)

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

To implement merge sort, we now only need a way to split the list in half. This is rather easy, we have already seen `drop` and `take`. An inbuilt function in Haskell is `splitAt :: Int -> [a] -> ([a], [a])` which is basically equivalent to `splitAt n xs = (take n xs, drop n xs) `. *An excercise would be to define this in a slightly more efficent manner and is left to the reader.* All in all, we can now merge sort using the function
```
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right) where
  (left,right) = splitAt (length xs `div` 2) xs
```




// cite
// citation 1
// citation 2
