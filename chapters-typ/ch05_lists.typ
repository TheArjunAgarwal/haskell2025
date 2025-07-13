#import "../Modules/Proof.typ" : proof
#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ": exercise
#import "../Modules/Tree.typ" : tree, far_away

A list is an ordered collection of objects, possibly with repetitions, denoted by
$
  [" object"_0" ", " object"_1" ", " object"_2" ", . . . " ", " object"_(n-1)" ", " object"_n" "]
$

These objects are called the *elements of the list*.

In Haskell, the elements of a particular list all have to have the same type.

Thus, a list such as
`[1,2,True,4]`
is not allowed.

= Type of List

If the elements of a list each have type `T`, then the list is given the type `[T]`.

```
>>> :t +d [1,2,3]
[1,2,3] :: [Integer]

>>> :t +d ['a','Z','\STX']
['a','Z','\STX'] :: [Char]

>>> :t +d [True,False]
[True,False] :: [Bool]
```

= Creating Lists

There are several nice ways to create a list in Haskell.

== Empty List

The most basic approach is to create the empty list by writing `[]`.

== Arithmetic Progression

Haskell has some luxurious syntax for declaring lists containing arithmetic progressions -

```
>>> [1..6]
[1,2,3,4,5,6]

>>> [1,3..6]
[1,3,5]

>>> [1,-3.. -10]
[1,-3,-7]

>>> [0.5..4.9]
[0.5,1.5,2.5,3.5,4.5]
```

But, very usefully, it just doesn't work for numbers, but other types as well.

```
>>> [False ..True]
[False,True]

>>> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
```

= Functions on Lists

Now that we know how to create a list, how do we manipulate them into the data that we would want?

= List Comprehension

Well, the way we achieve this in sets is through *set comprehension*.

When we want the set of squares of the even natural numbers $<=n$ , we write -
$
  { m^2 | m in { 0 , 1 , 2 , 3 , ... , n-1 , n } , 2 "divides" m }
$

Haskell lets us do the same with lists - 

```
>>> n = 10
>>> [ m*m | m <- [0..n] , m `mod` 2 == 0 ] 
[0,4,16,36,64,100]
```

When we want the set of pairs of numbers $<=n$ whose highest common factor is $1$, we write -
$
  { (x,y) | x , y in { 0 , 1 , 2 , 3 , ... ,n-1, n } , "HCF"(x,y) == 1}
$

,which can be expressed in haskell as

```
>>> n = 10
>>> [ (x,y) | x <- [1..n] , y <- [1..n] , gcd x y == 1 ]
[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10),(2,1),(2,3),(2,5),(2,7),(2,9),(3,1),(3,2),(3,4),(3,5),(3,7),(3,8),(3,10),(4,1),(4,3),(4,5),(4,7),(4,9),(5,1),(5,2),(5,3),(5,4),(5,6),(5,7),(5,8),(5,9),(6,1),(6,5),(6,7),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,8),(7,9),(7,10),(8,1),(8,3),(8,5),(8,7),(8,9),(9,1),(9,2),(9,4),(9,5),(9,7),(9,8),(9,10),(10,1),(10,3),(10,7),(10,9)]
```

== Cons or `(:)`

The operator `:` (read as "cons") can be used to add a single element to the the beginning of a list.

```
>>> 5 : [8,2,3,0]
[5,8,2,3,0]

>>> 1 : [2,3,4]
[1,2,3,4]

>>> 7 : [10,2,35,92]
[7,10,2,35,92]

>>> True : [False,True,True,False]
[True,False,True,True,False]
```

However, the `:` operator is much more special than it appears, since -
- It can be used to pattern match lists
- It is how lists are defined in the first place

So, how can we use it for pattern matching?

```
-- | pattern matching lists 
>>> (x:xs) = [5,8,3,2,0]
>>> x
5
>>> xs
[8,3,2,0]
```
When we use the pattern `(x:xs)` to refer to a list,
`x` refers to the first element of the list,\
and `xs` refers to the list containing the rest of the elements.


= Length

One of the most basic questions we could ask about lists is the number of elements they contain.\
The `length` function gives us that answers, counting repetitions as separate.

```
>>> length [5,5,5,5,5,5]
6

>>> length [5,8,3,2,0]
5

>>> length [7,10,2,35,92]
5

>>> length [False,True,True,False]
4
```

Ans we can use pattern matching to define it -

```
-- | length of list
length []     = 0
length (x:xs) = 1 + length xs
```
This reads - "
If the list is empty, then `length` is `0`.\
If the list has a first element `x`, then the `length` is `1 + length of the list of the rest of the elements`.
"

== Concatenate or `(++)`

The `++` (read as "concatenate") operator can be used to join two lists together.

```
>>> [5,8,2,3,0] ++ [122,32,44]
[5,8,2,3,0,122,32,44]

>>>  [False,True,True,False] ++ [True,False,True]
[False,True,True,False,True,False,True]
```
Again, we can define it by using pattern matching

```
-- | concatenation of lists
[]     ++ ys = ys
(x:xs) ++ ys = x : ( xs ++ ys )
```
This reads - "
Suppose we are concatenating a list to the front of the list `ys`.\
If the list is empty, then of course the answer is just `ys`.\
If the list has a first element `x`, and the rest of the elements form a list `xs`, then we can first concatenate `xs` and `ys`, and then add `x` at the beginning of the resulting list.
"

== Head and Tail

The `head` function gives the first element of a list.

```
>>> head [5,8,3,2,0]
5

>>> head [7,10,2,35,92]
7

>>> head [False,True,True,False]
False
```

And it can be defined using pattern-matching -
```
-- | head of list
head (x:xs) = x
```

The `tail` function provides the rest of the list after the first element.

```
>>> tail [5,8,3,2,0]     
[8,3,2,0]

>>> tail [7,10,2,35,92]
[10,2,35,92]

>>> tail [False,True,True,False]
[True,True,False]
```

And it can be defined using pattern-matching -
```
-- | tail of list
tail (x:xs) = xs
```

But how are these functions supposed to work if there is no first element at all, such as in the case of `[]`?
They produce errors when applied to the empty list! - 

```
>>> head []
*** Exception: Prelude.head: empty list
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\List.hs:1644:3 in base:GHC.List
  errorEmptyList, called at libraries\base\GHC\List.hs:87:11 in base:GHC.List
  badHead, called at libraries\base\GHC\List.hs:83:28 in base:GHC.List
  head, called at <interactive>:6:1 in interactive:Ghci6
```
```
>>> tail []
*** Exception: Prelude.tail: empty list
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\List.hs:1644:3 in base:GHC.List
  errorEmptyList, called at libraries\base\GHC\List.hs:130:28 in base:GHC.List
  tail, called at <interactive>:7:1 in interactive:Ghci6
```

Note that, in our definitions, we have not handled the case of the input being `[]`!

So, it is advised to use the function `uncons` from `Data.List`, which adopts the philosophy we saw in @code_of_function_to_a_maybe_type, which is 
$
  "if the function gives an error, output Nothing instead of the error"
$
Thus, for non-empty `l`, `uncons l` returns `Just (head l, tail l)`,
\ and when `l` is empty, `uncons l` returns `Nothing`.

Let's test this in GHCi - 

```
>>> import Data.List
>>> uncons [5,8,3,2,0]
Just (5,[8,3,2,0])
>>> uncons []         
Nothing
```

And the definition - 
```
-- | uncons of list
uncons []     = Nothing
uncons (x:xs) = Just ( x , xs )
```
Also consider the functions `safeHead` and `safeTail` from `Distribution.Simple.Utils`.

== Take and Drop

There are some "generalized" functions corresponding to `head` and `tail`, namely `take` and `drop`,

`take n l` gives the first `n` elements of `l`.

```
>>> take 3 [5,8,3,2,0]
[5,8,3]

>>> take 4 [7,10,2,35,92] 
[7,10,2,35]

>>> take 2 [False,True,True,False]
[False,True]
```

And the definition - 
```
-- | take from list
take 0   l    = []
take n (x:xs) = x : take (n-1) xs
take n []     = []
```
This reads - "
If we `take` only `0` elements, the result will of course be the empty list `[]`.\
If we want to take `n` elements, then we can take the first element and then the first `n-1` elements from the rest.\
But why the last line of the definition?
"
The last line of the function may look strange, but -
#exercise[
  Explain why, without the last line of the definition, the function might give an unexpected error.
]

`drop n l` gives `l`, excluding the first `n` elements.

```
>>> drop 3 [5,8,3,2,0]
[2,0]

>>> drop 4 [7,10,2,35,92]
[92]

>>> drop 2 [False,True,True,False]
[True,False]
```

And the definition - 
```
-- | drop from list
drop 0   l    = l
drop n (x:xs) = drop (n-1) xs
drop n []     = []
```
#exercise[
  Prove that the above definition works as told in the description of the functionality of the `drop` function.
]

The `splitAt` function combines these two functionalities by returning both answers in a pair.\ That is ; `splitAt n l == ( take n l , drop n l )`

```
>>> splitAt 3 [5,8,3,2,0]
([5,8,3],[2,0])
```

== Elem

The `elem` function takes a value and a list, and answers whether the value appears in the list or not, answering in either `True` or `False`.

```
>>> elem 5 [5,8,3,2,0]
True
>>> elem 8 [5,8,3,2,0] 
True
>>> elem 3 [5,8,3,2,0]
True
>>> elem 2 [5,8,3,2,0]
True
>>> elem 0 [5,8,3,2,0]
True
```

```
>>> elem 7 [5,8,3,2,0]
False
>>> elem 6 [5,8,3,2,0]
False
>>> elem 4 [5,8,3,2,0]
False
```

And the definition - 
```
elem x []     = False
elem x (y:ys) = x == y || elem x ys
```
This reads - "
`x` does not appear in the empty list.\
`x` appears in a list if and only if it is equal to the first element or it appears somewhere in the rest of the list.
"

== (!!)

The `!!` (read as bang-bang) operator takes a list and a number `n::Int`, and returns the $n^"th"$ element of the list, counting from `0` onwards.

```
>>> [5,8,3,2,0] !! 0
5
>>> [5,8,3,2,0] !! 1
8
>>> [5,8,3,2,0] !! 2
3
>>> [5,8,3,2,0] !! 3
2
>>> [5,8,3,2,0] !! 4
0
```


But what happens if `n` is not between `0` and `length l`?

Error! 

```
>>> [5,8,3,2,0] !! (-1)
*** Exception: Prelude.!!: negative index
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\List.hs:1369:12 in base:GHC.List
  negIndex, called at libraries\base\GHC\List.hs:1373:17 in base:GHC.List
  !!, called at <interactive>:8:13 in interactive:Ghci6

>>> [5,8,3,2,0] !! 5   
*** Exception: Prelude.!!: index too large
CallStack (from HasCallStack):
  error, called at libraries\base\GHC\List.hs:1366:14 in base:GHC.List
  tooLarge, called at libraries\base\GHC\List.hs:1376:50 in base:GHC.List
  !!, called at <interactive>:9:13 in interactive:Ghci6
```

So, again, it is advised to avoid using the `!!` operator.

#exercise[
  Provide a definition for the `!!` operator.
]

= Strings

A string is how we represent text (like English sentences and words) in programming.

Like many modern programming languages, Haskell defines a string to be just a list of characters.

In fact, the type `String` is just a way to refer to the actual type `[Char]`.

So, if we want write the text "hello there!", we can write it in GHCi as `['h','e','l','l','o',' ','t','h','e','r','e','!']`.

Let's test it out - 
```
>>> ['h','e','l','l','o',' ','t','h','e','r','e','!']
"hello there!"
```

But we see GHCi replies with something much simpler - `"hello there!"`

This simplified form is called syntactic sugar. It allows us to read and write strings in a simple form without having to write their actual verbose syntax each time.

So, we can write - 

```
>>> "hello there!"
"hello there!"

>>> :t +d "hello there!"
"hello there!" :: String
```

The type `String` is just a way to refer to the actual type `[Char]`.

And since strings are just lists, all the list functions apply to strings as well.

```
>>> 'h' : "ello there!"
"hello there!"

>>> "hello " ++ "there!"
"hello there!"

>>> head "hello there!"
'h'

>>> tail "hello there!"
"ello there!"

>>> take 5 "hello there!"
"hello"

>>> drop 5 "hello there!"
" there!"

>>> elem 'e' "hello there!"
True

>>> elem 'w' "hello there!"
False

>>> "hello there!" !! 7
'h'

>>> "hello there!" !! 6
't'
```

But there are some special functions just for strings -

`words` breaks up a string into a list of the words in it.
```
>>> words "hello there!"
["hello","there!"]
```
And `unwords` combines the words back into a single string.
```
>>> unwords ["hello","there!"]
"hello there!"
```
`lines` breaks up a string into a list of the lines in it.
```
>>> lines "hello there!\nI am coding..."
["hello there!","I am coding..."]
```
Ans `unlines` combines the lines back into a single string.
```
>>> unlines ["hello there!","I am coding..."]
"hello there!\nI am coding...\n"
```

= Structural Induction for Lists <indlist>

Suppose we wan prove some fact about lists.

We can use the following version of the @definition_of_principle_of_mathematical_induction - 

#def(sub:"structural induction for lists")[
  Suppose for each list `l` of type `[T]`, we have a statement $phi_(#[`l`])$.
  If we can pore the following two statements -
  - $phi_(#[`[]`])$
  - For each list of the form `(x:xs)`, if $phi_(#[`xs`])$ is true, then $phi_(#[`(x:xs)`])$ is also true.
  then $phi_(#[`l`])$ for all finite lists `l`.
]

Let use this principle to prove that 
#proof(thm:[The definition of `length` terminates on all finite lists.])[
  Let $phi_(#[`l`])$ be the statement 
  $
    "The definition of " #[`length l`] "terminates."
  $
  
  To use @definition_of_structural_induction_for_lists, we need to prove -
  
  - $angle.l angle.l phi_(#[`[]`]) angle.r angle.r$\
    The definition of `length []` directly gives `0`.
  
  - $angle.l angle.l$ For each list `(x:xs)`, if $phi_(#[`xs`])$, then $phi_(#[`(x:xs)`])$ also. $angle.r angle.r$\
    Assume $phi_(#[`xs`])$ is true.\
    The definition for `length (x:xs)` is `1 + length xs`.\
    By $phi_(#[`xs`])$,we know that `length xs` will finally give return some number `n`.\
    Therefore `1 + length xs` reduces to `1 + n`.\
    And `1 + n` obviously terminates.
]



= Optimization

Suppose we want to reverse the the order of elements in a list.

For example, transforming the list `[5,8,3,2,0]` into `[0,2,3,8,5]`.

So how do we define the function `reverse`?

An obvious definition is - 
```
-- | naive reverse
reverse []     = []
reverse (x:xs) = ( reverse xs ) ++ [x]
```

But this is not "optimal"?

What does this mean? Let's see -

Let's apply the definitions of `reverse` and `(++)` to see how `reverse [5,8,3]` is computed - 
```
reverse [5,8,3,2] == ( reverse [8,3]  ) ++ [5]

                  == ( ( reverse [3]  ) ++ [8] ) ++ [5]

                  == ( ( ( reverse [] ) ++ [3] ) ++ [8] ) ++ [5]

                  == ( ( [] ++ [3] ) ++         [8]   ) ++         [5]
                  
                  == (         [3]   ++         [8]   ) ++         [5]
                  
                  == (          3    :  ( [] ++ [8] ) ) ++         [5]
                  
                  == (          3    :          [8]   ) ++         [5]
                  
                  == (          3    : (        [8]     ++         [5] )
 
                  ==            3    : (         8      :  ( [] ++ [5] ) )

                  ==            3    : (         8      :          [5]   )

                  -- which finally is
                        [3,8,5]

```

So we see that this takes $10$ steps of computation.

Let us take an alternative definition of `reverse` - 
```
-- | optimized reverse
reverse l = help [] l where
    help xs (y:ys) = help (y:xs) ys
    help xs []     = xs
```

Let us how this one is computed step by step - 

```
reverse [5,8,3] == help      [] [5,8,3]

                == help     [5] [8,3]

                == help   [8,5] [3]

                == help [3,8,5] []

                == [3,8,5]
```

So we see this computation takes only $5$ steps, as compared to $10$ from last time.

So, in some way, the second definition is better as it requires much less steps.

We can comment on something similar for `splitAt`

```
-- | naive splitAt 
splitAt n l = ( take n l , drop n l )
```
```
-- | optimized splitAt 
splitAt n []     = []
splitAt n (x:xs) = ( x:ys , zs ) where
    (ys,zs) = splitAt (n-1) xs
```

#exercise[
  (1) Prove that the two definitions are equivalent using @definition_of_structural_induction_for_lists.\
  (2) See which definition takes more steps to compute `splitAt 2 [5,8,3]`
]

= Lists as Syntax Trees


Recall @definition_of_abstract_syntax_tree.

Remember that we represent $f(x,y)$ as #tree(($f$,$x$,$y$))

Using this rule, see whether the following steps make sense -

#[
#let t = it => tree(spread:1.5,it)
$
  #[`[5,8,3]`] &== #[`(:) 5 [8,3]`]\ \
  &== #t((`(:)`,`5`,`[8,3]`))\ \
  &== #t((`(:)`,`5`,`(:) 8 [3]`))\ \
  &== #t((`(:)`,`5`,(`(:)`,`8`,`[3]`)))\ \
  &== #t((`(:)`,`5`,(`(:)`,`8`,`(:) 3 []`)))\ \
  &== #t((`(:)`,`5`,(`(:)`,`8`,(`(:)`,`3`,`[]`))))
$]

In fact any list `[x1,x2,x3,...,xn]` can be represented as 
$ #tree((`(:)`,`x1`,(`(:)`,`x2`,(`(:)`,`x3`,(far_away(`(:)`),`xn`,`[]`))))) $

This is the representation that Haskell actually uses to store lists.

= Dark Magic

We can use our arithmetic progression notation to generate infinite arithmetic progressions.
```
>>> [0..]
[0,1,2,3,4,5,6,7,8,9,...]

>>> [2,5..]
[2,5,8,11,14,17,20,23,26,29,...]
```
We can define infinite lists like - 

a list of infinitely many `0`s -
```haskell
zeroes = 0 : zeroes
```
```
>>> zeroes
[0,0,0,0,0,0,0,0,0,0,...]
```


the list of all natural numbers - 
```haskell
naturals = l 0 where l n = n : l (n+1)
```
```
>>> naturals
[0,1,2,3,4,5,6,7,8,9,...]
```

and the list of all fibonacci numbers -
```haskell
fibs = l 0 1 where l a b = a : l b (a+b)
```
```
>>> fibs
[0,1,1,2,3,5,8,13,21,34,...]
```

Since we obviously cannot view the entirety of an infinite list, it is advisable to use `take` to view an initial section of the list, rather than the whole thing.

== Excercises

#exercise(sub : "Ballons")[
In an ICPC contest, balloons are distributed as follows: 
- Whenever a team solves a problem, that team gets a balloon.
- The first team to solve a problem gets an additional balloon.

A contest has 26 problems, labelled $A,B, dots, Z$. You are given the order of solved problems in the contest, denoted as a string $s$, where the $i$-th character indicates that the problem $s_i$ has been solved by some team. No team will solve the same problem twice.

Write a function `balloons :: String -> Int` to determine the total number of balloons used in the contest. Note that some problems may be solved by none of the teams.

Example : 

```
balloons "ABA" = 5
balloons "A" = 2
balloons "ORZ" = 6
balloons "BAAAA" = 7
balloons "BAAAA" = 7
balloons "BKPT" = 8
balloons "BKPT" = 8
balloons "HASKELL" = 13
```
]

#exercise(sub : "Neq Array (INOI 2025 P1)")[
Given a list $A$ of length $N$, we call a list of integers $B$ of length $N$ such that:
- All elemeents of $B$ are positive, ie $forall 1 <= i <= N, B_i > 0$
- $B$ is non-decreasing, ie $B_1 <= B_2 <= dots <= B_N$
- $forall 1 <= i <= N, B_i = A_i$

Let $op("neq") (A)$ denote the minimum possible value of the last element of $B$ for a valid array $B$.

Write a function `neq :: [Int] -> Int` that takes a list $A$ and returns the $op("neq") (A)$.

Example : 
```
neq [2,1] = 2
neq [1,2,3,4] = 5
neq [2,1,1,3,2,1] = 3
```
]

#exercise(sub : "Kratki (COCI 2014)")[
Given two integers $N$ and $K$, write a function `krat :: Int -> Int -> Maybe [Int]` which constructs a permutation of numbers from $1$ to $N$ such that the length of its longest monotone subsequence (either ascending or descending) is exactly $K$ or declare that the following is not possible.

A monotone subsequence is a subsequence where elements are either in non-decreasing order (ascending) or non-increasing order (descending).


Example:
```
krat 4 3 = Just [1,4,2,3]
krat 5 1 = Nothing
krat 5 5 = Just [1,2,3,4,5]
```
For example 1: The permutation (1, 4, 2, 3) has longest ascending subsequence (1, 2, 3) of length 3, and no longer monotone subsequence exists.
For example 2: It's impossible to create a permutation of 5 distinct numbers with longest monotone subsequence of length 1.
For example 3: The permutation (1, 2, 3, 4, 5) itself is the longest monotone subsequence of length 5.
]

#exercise(sub : "Putnik (COCI 2013")[
  Chances are that you have probably already heard of the travelling salesman problem. If you have, then you are aware that it is an NP-hard problem because it lacks an efficient solution. Well, this task is an uncommon version of the famous problem! Its uncommonness derives from the fact that this version is, actually, solvable.

  Our vacationing mathematician is on a mission to visit $N$ cities, each exactly once. The cities are represented by numbers $1, 2, dots, N$. What we know is the direct flight duration between each pair of cities. The mathematician, being the efficient woaman that she is, wants to modify the city visiting sequence so that the total flight duration is the minimum possible.

  Alas, all is not so simple. In addition, the mathematician has a peculiar condition regarding the sequence. For each city labeled $K$ must apply: either all cities with labels smaller than $K$ have been visited before the city labeled $K$ or they will all be visited after the city labeled $K$. In other words, the situation when one of such cities is visited before, and the other after is not allowed.
  
  Assist the vacationing mathematician in her ambitious mission and write a function `time :: [[Int]] -> Int` to calculate the minimum total flight duration needed in order to travel to all the cities, starting from whichever and ending in whichever city, visiting every city exactly once, so that her peculiar request is fulfilled.
Example : 
```
time [
  [0,5,2],
  [5,0,4], 
  [2,4,0]] = 7


time [
  [0,15,7,8],
  [15,0,16,9],
  [7,16,0,12],
  [8,9,12,0]] = 31
]
```
In the first example: the optimal sequence is 2, 1, 3 or 3, 1, 2. The sequence 1, 3, 2 is even more favourable, but it does not fulfill the condition.
In the second example: the sequence is either 3, 1, 2, 4 or 4, 2, 1, 3.
]

#exercise(sub : "Look and Say")[
Look-and-say sequences are generated iteratively, using the previous value as input for the next step. For each step, take the previous value, and replace each run of digits (like 111) with the number of digits (3) followed by the digit itself (1).

For example:

- $1$ becomes $11$ ($1$ copy of digit $1$).
- $11$ becomes $21$ ($2$ copies of digit $1$).
- $21$ becomes $1211$ (one $2$ followed by one $1$).
- $1211$ becomes $111221$ (one $1$, one $2$, and two $1$s).
- $111221$ becomes $312211$ (three $1$s, two $2$s, and one $1$).

Write function `lookNsay :: Int -> Int` which takes an number and generates the next number in its look and say sequence.
]

#exercise(sub : "Triangles (Codeforces)")[
  Pavel has several sticks with lengths equal to powers of two. He has $a_0$ sticks of length $2^0=1$, $a_1$ sticks of length $2^1 = 2$, ..., $a_n$ sticks of length $2^n$.
  
  Pavel wants to make the maximum possible number of triangles using these sticks. The triangles should have strictly positive area, each stick can be used in at most one triangle.
  
  It is forbidden to break sticks, and each triangle should consist of exactly three sticks. Write a function `triangles :: [Int] -> Int]` to find the maximum possible number of triangles.

Examples
```
triangles [1,2,2,2,2] = 3
triangles [1,1,1] = 0
triangles [3,3,3] = 1
```
In the first example, Pavel can, for example, make this set of triangles (the lengths of the sides of the triangles are listed): $(2^0,2^4,2^4), (2^1,2^3,2^3), (2^1,2^2,2̂2)$.

In the second example, Pavel cannot make a single triangle.

In the third example, Pavel can, for example, create this set of triangles (the lengths of the sides of the triangles are listed): $(2^0,2^0,2^0), (2^1,2^1,2^1), (2^2,2^2,2^2)$.
]

#exercise(sub : "Thanos Sort (Codeforces)")[
  Thanos sort is a supervillain sorting algorithm, which works as follows: if the array is not sorted, snap your fingers\* to remove the first or the second half of the items, and repeat the process.
  
  Given an input list, what is the size of the longest sorted list you can obtain from it using Thanos sort? Write function `thanos :: Ord a => [a] -> Int` to determine that.

\* Infinity Gauntlet required.


Examples
```
thanos [1,2,2,4] = 4
thanos [11, 12, 1, 2, 13, 14, 3, 4] = 2
thanos [7,6,5,4] = 1
```

In the first example the list is already sorted, so no finger snaps are required.

In the second example the list actually has a subarray of 4 sorted elements, but you can not remove elements from different sides of the list in one finger snap. Each time you have to remove either the whole first half or the whole second half, so you'll have to snap your fingers twice to get to a 2-element sorted list.

In the third example the list is sorted in decreasing order, so you can only save one element from the ultimate destruction.
]

#exercise(sub : "Deadfish")[
Deadfish XKCD is a fun, unusual programming language. It only has one variable, called $s$, which starts at $0$. You change $s$ by using simple commands.

Your task is to write a Haskell program that reads Deadfish XKCD code from a file, runs it, and prints the output.

Deadfish XKCD has the following commands:

#table(
  columns: 2,
  [*Command*], [*What It Does*],
  [x], [Add $1$ to $s$.],
  [k], [print $s$ as a number.],
  [c], [Square $s$,],
  [d], [Subtract $1$ from $s$.],
  [X], [Start defining a function (the next character is the function name).],
  [K], [Print $s$ as an ASCII character.],
  [C], [End the function definition or run a function.],
  [D], [Reset $s$ back to 0.],
  [{}], [Everything inside curly braces is considered a comment.]
)

Extra Rules:

- $s$ must stay between $0$ and $255$.
- If $s$ goes above $255$ or below $0$, reset it back to $0$.
- Ignore spaces, newlines, and tabs in the code.
- Other characters (not commands) work differently depending on the subtask.

While you can do the whole exercise in one go, we reccomend doing the following subtasks in order.

1. Basic (x, k, c, d only)
`run "xxcxkdk" = "54"`

2. Extended (add K, D)
`run "xxcxxxxcxxxxxxxxK" = "H"`

3. Functions (all commands)
`run "XUxkCxxCUCUCU" = "345"`

4. Comments (ignore {})
Ignore content inside curly braces.

*Hint*: Don't be afraid to use tuples!
]

#exercise(sub : "Weakness and Poorness (Codeforces)")[
You are given a sequence of $n$ integers $a_1, a_2,dots, a_n$.

Write a function `solve :: [Int] -> Float` to determine a real number $x$ such that the weakness of the sequence $a_1 - x, a_2 - x, dots , a_n - x$ is as small as possible.

The weakness of a sequence is defined as the maximum value of the poorness over all segments (contiguous subsequences) of a sequence.

The poorness of a segment is defined as the absolute value of sum of the elements of segment.

Examples
```
solve [1,2,3] = 2.0
solve [1,2,3.4] = 2.5
```
Note
For the first case, the optimal value of $x$ is $2$ so the sequence becomes  $- 1, 0, 1$ and the max poorness occurs at the segment $-1$ or segment $1$.

For the second sample the optimal value of $x$ is $2.5$ so the sequence becomes  $- 1.5,  - 0.5, 0.5, 1.5$ and the max poorness occurs on segment $-1.5, -0.5$ or $0.5, 1.5$.
]

