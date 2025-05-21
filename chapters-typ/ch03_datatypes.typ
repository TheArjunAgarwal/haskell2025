#import "../Definition.typ" : def

#let definition = def
#let example = it => [For example - \ #it]
#let exercise = it => [=== Exercise 
#it]

= Bool, Int, Integer and more (feel free to change it)

== Introduction to Types
Haskell is a strictly typed language. This means, Haskell needs to strictly know what the type of *anything* and everything is.

But one would ask here, what is type? According to Cambridge dictionary, 
#def[*Type* is refers to a particular group of things that share similar characteristics and form a smaller division of a larger set]

//suggest that we do not define type

Haskell being strict implies that it needs to know the type of everything it deals with. For example,
- The type of $e$ is *`Real`*.
- The type of $2$ is *`Int`*, for integer.
- The type of $2$ can also be *`Real`*. But the `2 :: Int` and `2 :: Real` are different, because they have different types.
- The type of $x |-> floor(x)$ is *`Real -> Int`*, because it takes a real number to an integer.
- We write $(x |-> floor(x)) e = 2$ By applying a function of type *`Real -> Int`* to something of type *`Real`* we get something of type *`Int`*
- The type of $x |-> x + 2$, when it takes integers, is *`Int -> Int`*.
- We cannot write $(x |-> x + 2)(e)$, because the types don’t match. The function wants an input of type *`Int`* but $e$ is of type *`Real`*. We could define a new function $x |-> x+2$ of type *`Real -> Real`*, but it is a different function.
- Functions can return functions. Think of $(+)$ as a function that takes an *`Int`*, like $3$, and returns a function like $x |-> x + 3$, which has type *`Int -> Int`* Concretely, $(+)$ is $x |-> (y |-> y + x)$. This has type *`Int -> (Int -> Int)`*.
- We write $(+)(3)(4) = 7$. First, $(+)$ has type *`Int -> (Int -> Int)`*, so $(+)(3)$ has type *`Int -> Int`*. So, $(+)(3)(4)$ should have type *`Int`*.
- The type of $x |-> 2*x$ is *`Int -> Int`* when it takes integers to integers. It can also be *`Real -> Real`* when it takes reals to reals. These are two different functions, because they have different types. But if we make a 'super type' or *typeclass* /*suggest that we not mention typeclass yet*/ called *`Num`* is which is a property which both *`Int`* and *`Real`* have, then we can define $x |-> 2*x$ more generally as of type *`Num a => a -> a`* which reads, for a type *`a`* with property(belonging to) *`Num`*, the function $x |-> 2*x$ has type *`a -> a`*
- Similarly, one could define a generalized version of the other functions we described.

//suggest that we not mention type theory
A study of types and what we can infer from them(and how we can infer them) is called, rightfully so, *Type Theory*. It is deeply related to computational proof checking and formal verification. While we will not study about it in too much detail in this course, it is its own subject and is covered in detail in other courses.

While we recommend, atleast for the early chapters, to declare the types of your functions explicitly ex. `(+) :: Int -> Int  -> Int`; Haskell has a type inference system#footnote[Damas–Hindley–Milner Type Inference is the one used in Haskell at time of writing.] which is quite accurate and tries to go for the most general type. This can be both a blessing and curse, as we will see in a few moments.

//suggest that we not mention `Float` at all, since we don't go into any kind of explanation of floating point, so I don't think students can be expected to understand enough to justify mentioning it. I  would personally say that I don't like `Double` either as it is not perfectly a real number.
This chapter will deal (in varying amounts of details) with the types *`Bool`*, *`Int`*, *`Integer`*, *`Float`*, *`Char`* and *`String`*.
#def[
  *`Bool`* is a type which has only two valid values, *`True`* and *`False`*. It most commonly used as output for indicator functions(indicate if something is true or not).
]
// suggest that we don't explain `Int` so deeeply, move extra info to appendix
#def[
  *`Int`* and *`Integer`* are the types used to represent integers. 

  `Integer` can hold any number no matter how big, up to the limit of your machine's memory, while `Int` corresponds to the set of positive and negative integers that can be expressed in 32 or 64 bits(based on system) with the bounds changing depending on implementation (guaranteed at least -2^29 to 2^29). Going outside this range may give weird results. /*why not us #ex*/ Ex. /*Do we expect them to know `product at this point?`*/`product [1..52] :: Int` gives a negative number which cannot realistically be $52!$. On the other hand, `product [1..52] :: Integer` gives indeed the correct answer.

  The reason for `Int` existing despite its bounds and us not using `Integer` for everything is related to speed and memory. Using the former is faster and uses lesser memory.

  ```
  ghci> product [1..52] :: Int
  -8452693550620999680
  (0.02 secs, 87,896 bytes)
  ghci> product [1..52] :: Integer
  80658175170943878571660636856403766975289505440883277824000000000000
  (0.02 secs, 123,256 bytes)
  ```
  Almost 1.5 times more memory is used in this case.
]

An irrefutable fact is that computers are fundamentally limited by the amount of data they can keep and humans are fundamentality limited by the amount of time they have. This implies that if, we can optimize for speed and space, we should do so. We will talk some more about this in [chapter 9], but the rule of thumb is that more we know about the input, the more we can optimize. Knowing that it will be between, say $-2^29$ to $2^29$, allows for some optimizations which can't be done with arbitrary length. We (may) see some of these optimizations later.

#def[
  *`Rational`*, *`Float`* and *`Double`* are the types used to deal with non-integral numbers. The former is used for fractions or rationals while the latter for reals with varying amount of precision. Rationals are declared using `%` as the viniculum(the dash between numerator and denominator). For example `1%3, 2%5, 97%31`.

//suggest that we not mention `Float` at all
  `Float` or Floating point contains numbers with a decimal point with a fixed amount of memory being used for their storage. The term floating-point comes from the fact that the number of digits permitted after the decimal point depends upon the magnitude of the number. The same can be said for `Double` or Double Precision Floating Point which offers double the space beyond the point, at cost of more memory. For example
  ```
  ghci> sqrt 2 :: Float
  1.4142135
  ghci> sqrt 99999 :: Float
  316.2262
  ghci> sqrt 2 :: Double
  1.4142135623730951
  ghci> sqrt 99999 :: Double
  316.226184874055
  ghci> sqrt 999999999 :: Double
  31622.776585872405
  ```
We can see that the prescission of $sqrt(99999)$ is much lower than that of $sqrt(2)$. We will use `Float` for most of this book. //please no
]


#def[
  *`Char`* are the types used to represent arbitrary Unicode characters. This includes all numbers, letters, white spaces(space, tab, newline etc) and other special characters.

  *`String`* is the type used to represent a bunch of characters chained together. Every word, sentence, paragraph is either a string or a collection of them.

  In haskell, Strings and Chars are differentiated using the type of quotation used. `"hello" :: String` as well as `"H" :: String` but `'H':: Char`. Unlike some other languages, like say Python, we can't do so interchangeably. Double Quotes for Strings and Single Quotes for Chars.
]
Similer to many modern languages, In Haskell, String is just a synonym for a list of characters that is `String` is same as `[Char]`. This allows string manipulation to be extremely easy in Haskell and is one of the reason why Pandoc, a universal document converter and one of the most used software in the world, is written in Haskell. We will try to make a mini version of this at the end of the chapter.

// why define tuples here, when it'll be defined in chp4?
#def[
  To recall/*from where?*/, a tuple is a length immutable, ordered multi-typed data structure. This means we can store a fixed number of multiple types of data in an order using tuples. Ex.
  `(False , True ) :: (Bool, Bool)`
  `(False , 'a', True ) :: (Bool, Char, Bool)` 
  `("Yes", 5.21 , 'a') :: (String, Float, Char)`
  
  A list is a length mutable, ordered, single typed data structure. This means we can store an arbitrary number things of the same type in a certain order using lists. Ex.
  `[False, True, False] :: [Bool]`
  `['a','b','c','d'] :: [Char]`
  `["One","Two","Three"] :: [String]`
]

== Logical Operations //suggestion to add a definitio of `logical operator` for sanity purposes
#example[
  Write Haskell code to simulate the following logical operators
  + NOT
  + OR
  + AND
  + NAND
  + XOR
]
Implementing a not operator seems the most straightforward and it indeed is. We can simply specify the output for all the cases, as there are only 2.
```
not :: Bool -> Bool
not True = False
not False = True
```
The inbuilt function is also called `not`.
We could employ a smiler strategy for `or` to get the following code
```
or :: Bool -> Bool -> Bool
or True True = True
or True False = True
or False True = True
or False False = False
```
but this is too verbose. One could write a better code using wildcards as follows
```
or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True
```
As the first statement is checked against first, the only  false case is evaluated and if it is not satisfied, we just return true. We can write this as a one liner using the if statement.
```
or :: Bool -> Bool -> Bool
or a b = if (a,b) == (False, False) then False else True
```
The inbuilt operator for this is `||` used as `False || True` which evaluates to `True`.

How would one write such a code for `and`? This is left as exercise for the reader. The inbuilt operator for this is `&&` used as `True && False` which evaluates to `False`.

Now that we already have `and` and `not`, could we make `nand` by just composing them? Sure.
```
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)
```
This also seems like as good of a time as any to introduce operation conversion and function composition. In Haskell, functions are first class citizens. It is a functional programming language after all. Given two functions, we naturally want to compose them. Say we want to make the function $h(x) : x |-> -x^2$ and we have $g(x) : x |-> x^2$ and $f(x) : x |-> -x$. So we can define $h(x) := (f compose  g)(x) = f(g(x))$. In haskell, this would look like
```
negate :: Int -> Int
negate x = - x

square :: Int -> Int
square x = x^2

negateSquare :: Int -> Int
negateSquare x = negate . square
```
We could also define `negateSquare` in a more cumbersome `negateSquare x = negate(square x)` but with complicated expressions these brackets will add up and we want to avoid them as far as possible. We will also now talk about the fact that the infix operators, like `+, -, *, /, ^, &&, ||` etc are also deep inside functions. This means we can should be able to access them as functions(to maybe compose them) as well as make our own. And we indeed can, the method is brackets and backticks.

An operator inside a bracket is a function and a function in backticks is an operator. For example
```
ghci> True && False
False
ghci> (&&) True False
False
ghci> f x y = x*y + x + y
ghci> f 3 4
19
ghci> 3 `f` 4
19
```
All this means, we could define `nand` simply as
```
nand :: Bool -> Bool -> Bool
nand = not . (&&)
```
Furthermore, as Haskell doesn't have an inbuilt nand operator, say I want to have `@@` to represent it. Then, I could write
```
(@@) :: Bool -> Bool -> Bool
(@@) = not.(&&)
```
// is this definition correct?

Finally, we need to make `xor`.  We will now replicate a classic example of 17 ways to define it and a quick reference for a lot of the syntax. 
```
-- 17 Xors
-- Notice, we can declare the type of a bunch of functions by comma seperating them.

xor1, xor2, xor3, xor4, xor5 :: Bool -> Bool -> Bool

-- Explaining the output for each and every case.
xor1 False False = False
xor1 False True = True
xor1 True False = True
xor1 True True = False

-- We could be smarter and save some keystrokes
xor2 False b = b
xor2 b False = b
xor2 b1 b2 = False

-- This seems to to be the same length but notice, b1 and b2 are just names never used again. This means..
xor3 False True = True
xor3 True False = True
xor3 b1 b2 = False

-- .. we can replace them with wildcards.
xor4 False True = True
xor4 True False = True
xor4 _ _ = False


-- Although, a simple observation recduces work further. Notice, we can't replace b with a wild card here as it is used in the defination later and we wish to refer to it.
xor5 False b = b
xor5 True b = not b
```
All the above methods basically enumerate all possibilities using increasingly more concise manners. However, can we do better using logical operators?

```
-- 17 Xors contd.
xor6, xor7, xor8, xor9 :: Bool -> Bool -> Bool
-- Litrally just using the definition
xor6 b1 b2 = (b1 && (not b2)) || ((not b1) && b2)

-- Recall that the comparision operators return bools?
xor7 b1 b2 = b1 /= b2

-- And using the fact that operators are functions..
xor8 b1 b2 = (/=) b1 b2

-- .. we can have a 4 character definition.
xor9 = (/=)
```
We could also use `if..then..else` syntax. To jog your memory, the `if` keyword is followed by some condition, aka a function that returns `True` or `False`, this is followed by the `then` keyword and a function to excute if the condition is satisfied and the `else` keyword and a function to execute as a if the condition is not satisfied. For example
```
-- 17 Xors, contd.
xor10, xor11 :: Bool -> Bool -> Bool

xor10 b1 b2 = if b1 == b2 then False else True

xor11 b1 b2 = if b1 /= b2 then True else False
```

Or use the guard syntax. Similar to piecewise functions in math, we can define the function piecewise with the input changing the definition of the function, we can define guarded definition where the inputs control which definition we access. If the pattern(a condition) to a guard is met, that definition is accessed in order of declaration.

We do this as follows

```
-- 17 Xors, cotd
xor12, xor13, xor14, xor15 :: Bool -> Bool -> Bool

xor12 b1 b2
  | b1 == True = not b2 -- If b1 is True, the code acesses this definition regardless of b2's value. The function enters the definition which matches first.
  | b2 == False = b1
  
-- Can you spot a problem in xor12? xor12 False True is not defined and would raise the exception Non-exhaustive patterns in function xor12.
-- This means that the pattern of inputs provided can't match with any of the definitions. We can fix it by either being careful and matching all the cases..

xor13 False b2 = b2 -- Notice, we can have part of the definition unguarded before entering the guards.
xor13 True b2
  | b2 == False = True
  | b2 == True = False

xor14 b1 b2
  | b1 == b2 = False
  | b1 /= b2 = True

-- .. or by using the otherwise keyword, we can define a catch-all case. If none of the patterns are matched, the function enters the otherwise definiton.

xor15 b1 b2
  | b1 == True = not b2
  | otherwise = b2

```
Finally, we can define use the `case .. of ..` syntax. While this syntax is rarer, and too verbose, for simple functions, we will see a lot of it later in [monads chapter]/*how ?*/. In this syntax, the general form is
```
case <expression> of
  <pattern1> -> <result1>
  <pattern2> -> <result2>
  ...
```
The case expression evaluates the `<expression>`, and matches it against each pattern in order. The first matching pattern's corresponding result is returned. You can nest case expressions to match on multiple values, although it can become extreamly unreadable, rather quickly.


```
-- 17 Xors, contd
xor16, xor17 :: Bool -> Bool -> Bool

-- We use a single case on the first input.
xor16 :: Bool -> Bool -> Bool
xor16 b1 b2 = case b1 of
  False -> b2
  True -> not b2

-- Or we can return to defining for every single case, just using more words.
xor17 b1 b2 = case b1 of
  False -> case b2 of
    False -> False
    True -> True
  True -> case b2 of
    False -> True
    True -> False

```
Now that we are done with this tiresome activity, and learned a lot of Haskell syntax, let's go for a ride.

#exercise[
  It is a well know fact that one can define all logical operators using only `nand`. Well, let's do so. Redefine `and, or, not, xor` using only `nand`.
]

== Numerical Functions
A lot of numeric operators and functions come predefined in Haskell. Some natural ones are
```
ghci> 7 + 3
10
ghci> 3 + 8
11
ghci> 97 + 32
129
ghci> 3 - 7
-4
ghci> 5 - (-6)
11
ghci> 546 - 312
234
ghci> 7 * 3
21
ghci> 8*4
32
ghci> 45 * 97
4365
ghci> 45 * (-12)
-540
ghci> (-12) * (-11)
132
ghci> abs 10
10
ghci> abs (-10)
10
```
The internal definition of addition and subtraction is discussed in the appendix while we talk about some multiplication algorithms in the time complexity chapter. For our purposes, we want it to be clear and predictable what one expects to see when any of these operators are used. `Abs` is also implemented in a very simple fashion.
```
-- Implementation of abs function
abs :: Num a => a -> a
abs a = if a >= 0 then a else -a
```
=== Division, A Trilogy
Now let's move to the more interesting operators and functions. 

`recip` is a function which reciptocates a given numebr, but it has rather interesting type signature. It is only defined on types with the `Fractional` typeclass. /*suggest that we not mention typeclass yet*/ This refers to a lot of things, but the most common ones are `Rational, Float` and `Double`.  `recip`, as the name suggests, returns the reciprocal of the number taken as input. The type signature is `recip :: Fractional a => a -> a`
```
ghci> recip 5
0.2
ghci> k = 5 :: Int
ghci> recip k
<interactive>:47:1: error: [GHC-39999]...
```
It is clear that in the above case, 5 was treated as a `Float` or `Double` and the expected output provided. In the following case, we specified the type to be `Int` and it caused a horrible error. This is because for something to be a fractional type, we literally need to define how to reciprocate it. We will talk about how exactly it is defined in < some later chapter probably 8 >. For now, once we have `recip` defined, division can be easily defined as
```
(/) :: Fractional a => a -> a -> a
x / y = x * (recip y)
```
Again, notice the type signature of `(/)` is `Fractional a => a -> a -> a`. #footnote("It is worth pointing out that one could define `recip` using `(/)` as well given 1 is defined. While this is not standard, if `(/)` is defined for a data type, Haskell does autmoatically infer the reciprocation. So technically, for a datatype to be a memeber of the type class `Fractional` it needs to have either reciprocation or division defined, the other is infered.")

However, this is not the only division we have access to. Say we want only the quotient, then we have `div` and `quot` functions. These functions are often coupled with `mod` and `rem` are the respective remainder functions. We can get the quotient and remainder at the same time using `divMod` and `quotRem` functions. A simple example of usage is
```
ghci> 100 `div` 7
14
ghci> 100 `mod` 7
2
ghci> 100 `divMod` 7
(14,2)
ghci> 100 `quot` 7
14
ghci> 100 `rem` 7
2
ghci> 100 `quotRem` 7
(14,2)
```
One must wonder here that why would we have two functions doing the same thing? Well, they don't actually do the same thing.

#exercise[
  From the given example, what is the difference between `div` and `quot`?
  ```
ghci> 8 `div` 3
2
ghci> (-8) `div` 3
-3
ghci> (-8) `div` (-3)
2
ghci> 8 `div` (-3)
-3
ghci> 8 `quot` 3
2
ghci> (-8) `quot` 3
-2
ghci> (-8) `quot` (-3)
2
ghci> 8 `quot` (-3)
-2
```
]

#exercise()[
  From the given example, what is the difference between `mod` and `rem`?
  ```
ghci> 8 `mod` 3
2
ghci> (-8) `mod` 3
1
ghci> (-8) `mod` (-3)
-2
ghci> 8 `mod` (-3)
-1
ghci> 8 `rem` 3
2
ghci> (-8) `rem` 3
-2
ghci> (-8) `rem` (-3)
-2
ghci> 8 `rem` (-3)
2
```
]

While the functions work similerly when the divisior and dividend are of the same sign, they seem to diverge when the signs don't match. The thing here is we ideally want our division algorithm to satisfy $d * q + r = n, |r| < |d|$ where $d$ is the divisior, $n$ the dividend, $q$ the quotient and `r` the remainder. The issue is for any $- d < r < 0 => 0 < r < d$. This means we need to choose the sign for the remainder. 

In Haskell, `mod` takes the sign of the divisor(comes from floored division, same as Python's `%`), while `rem` takes the sign of the dividend (comes from truncated division, behaves the same way as Scheme's `remainder` or C's `%`.).

Basically, `div` returns the floor of the true divison value(recall $floor(-3.56) = -4$) while `quot` returns the trunicated value of the true division(recall $op("trunicate")(-3.56) = -3$ as we are just trunicating the decimal point off). The reason we keep both of them in Haskell is to be comfertable for people who come from either of these languages. Also, The `div` function is often the more natural one to use, whereas the `quot` function corresponds to the machine instruction on modern machines, so it's somewhat more efficient(although not much, I had to go upto $10^100000$ to even get millisecond difference in the two).

A simple excercise for us now would be implementing our very own integer division algorithm. We begin with a division algorithm for only positive integers.
```
-- A division algorithm on positive integers by repreated subtraction
divide :: Integer -> Integer -> (Integer, Integer)
divide n d = go 0 n where
  go q r = if r >= d then go (q+1) (r-d) else (q,r)
```
Now, how do we extend it to negitives by a little bit of case handling.
```
divideComplete :: Integer -> Integer -> (Integer, Integer)
divideComplete _ 0 = error "DivisionByZero"
divideComplete n d
  | d < 0     = let (q, r) = divideComplete n (-d) in (-q, r)
  | n < 0     = let (q, r) = divideComplete (-n) d in if r == 0 then (-q, 0) else (-q - 1, d - r)
  | otherwise = divideUnsigned n d

divide :: Integer -> Integer -> (Integer, Integer)
divide n d = go 0 n where
  go q r = if r >= d then go (q+1) (r-d) else (q,r)
```

An excercise left for the reader is to figure out which kind of division is this, floored or trunicated, and implement the one we haven't yourself. Let's now tal

=== Exponantion
Haskell defines for us three exponation operators, namely `(^^), (^), (**)`. 

#exercise[
  What can we say about the three exponation operators?
  ```
<will make this example later>

  ```
]

Unlike division, they have almost the same function. The difference here is in the type signature. While, infering the exact type signature was not expected, we can notice:
- `^` is raising genral numbers to positive integral powers. This means it makes no assumptions about if the base can be reciprocated and just produces an error if the power is negative.
- `^^` is raising fractional numbers to general integral powers. That is, it needs to be sure that the reciprocal of the base exists(negative powers) and doesn't throw an error if the power is negative.
- `**` is raising numbers with floating point to powers with floating point. This makes it the most general exponation.

The operators clearly get more and more general as we go down the list but they also get slower. However, they are also reducing in accurecy and may even output `Infinity` in some cases. The `...` means I am trunicating the output for readablity, ghci did give the compelete answer.

```
ghci> 2^1000
10715086071862673209484250490600018105614048117055336074...
ghci> 2 ^^ 1000
1.0715086071862673e301
ghci> 2^10000
199506311688075838488374216268358508382...
ghci> 2^^10000
Infinity
ghci> 2 ** 10000
Infinity
```

The exact reasons for the inaccuracy comes from float conversions and approximation methods. We will talk very little about this specialist topic somewhat later. 

However, something within our scope is implementing `(^)` ourselves.

```
-- A naive integer exponation algorithm
exponation :: (Num a, Integral b) => a -> b -> a
exponation a 0 = 1
exponation a b = if b < 0 
  then error "no negitve exponation" 
  else a * (exponation a (b-1))
```
This algorithm, while the most naive way to do so, computes $2^100000$ in mearly $0.56$ seconds.

However, we could do a bit better here. Notice, to evaluate $a^b$, we are making $b$ multiplications. A fact we mentioned before is that multiplication of big numbers is faster when it is balenced, that is the numbers being multiplied have similer number of digits.

So to do better, we could simply compute $a^(b/2)$ and then square it, given $b$ is even, or compute $a^((b-1)/2)$ and then square it and multiply by $a$ otherwise. This can be done recusrsivly till we have the solution. 
```
-- A better exponentiation algorithm using divide and conquer
exponation :: (Num a, Integral b) => a -> b -> a
exponation a 0 = 1
exponation a b 
  | b < 0     = error "no negitve exponation"
  | even b    = let half = exponation a (b `div` 2)
                in half * half
  | otherwise = let half = exponation a (b `div` 2)
                in a * half * half
```
The idea is simple: instead of doing $b$ multiplications, we do far fewer by solving a smaller problem and reusing the result. While one might not notice it for smaller $b$'s, once we get into the hundreds or thousands, this method is dramatically faster.

This algorithm brings the time to compute $2^100000$ down to $0.07$ seconds. 

The idea is that we are now making atmost $3$ multiplications at each step and there are atmost $log(b)$ steps. This brings us down from $b$ multiplications to $3 log(b)$ multiplications. Furthermore, most of these multiplications are somewhat balenced and hence optimized.

This kind of a stratergy is called divide and conquer. You take a big problem, slice it in half, solve the smaller version, and then stitch the results together. It’s a method/technique that appears a lot in Computer Science(in sorting to data search to even solving diffrential equations and training AI models) and we will see it again shortly.

Finally, there’s one more minor optimization that’s worth pointing out. It's a small thing, and doesn't even help that much in this case, but if the multiplication were particularly costly, say as in matrices; our exponation method could be made slightlty better. Let's say we are dealing with say $2^255$. Our current algorithm would evaluate it as:
$
2^31 
= (2^15)^2 * 2\
= ((2^7)^2 * 2)^2 * 2\
= (((2^3)^2 * 2)^2 * 2)^2 * 2\
= ((((2^1)^2 * 2)^2 * 2)^2 * 2)^2 * 2\
$
This is a problem as the small $*2$ in every bracket are unbalenced. The exact way we deal with all this is by something called #link("en.wikipedia.org/wiki/Exponentiation_by_squaring#2k-ary_method")[2^k arry method]. Although, more often then not, most built in implementations use the divide and conquer exponentiation we studied.

=== A short tour of number theoretic functions
A very common function for number theoretic use cases is `gcd` and `lcm`. They are pre-defined as
```
ghci> :t gcd
gcd :: Integral a => a -> a -> a
ghci> :t lcm
lcm :: Integral a => a -> a -> a
ghci> gcd 12 30
6
ghci> lcm 12 30
60
```
We will now try to define them ourselves. The idea here to use the Euclid's algorithm.




=== Mathematical Functions
We will now talk about mathematical functions like `log`, `sqrt`, `sin`, `asin` etc. We will also take this oppurtunity to talk (very briefly) about real exponation. To begin, Haskell has a lot of pre-defined functions.

```
ghci> pi
3.141592653589793
ghci> sin pi
1.2246467991473532e-16
ghci> cos pi
-1.0
ghci> tan pi
-1.2246467991473532e-16
ghci> asin 1
1.5707963267948966
ghci> asin 1/2
0.7853981633974483
ghci> acos 1
0.0
ghci> atan 1
0.7853981633974483

ghci> log (2.71818)
0.9999625387017254
ghci> log 4
1.3862943611198906
ghci> log 100
4.605170185988092
ghci> logBase 10 100
2.0
ghci> exp 1
2.718281828459045
ghci> exp 10
22026.465794806718

ghci> sqrt 81
9.0
```
`pi` is a predefined variable inside haskell. It carries the value of $pi$ upto some decimal places based on what type it is forced in.
```
ghci> a = pi :: Float
ghci> a
3.1415927
ghci> b = pi :: Double
ghci> b
3.141592653589793
```
All the fucntions above have the type signature `Fractional a => a -> a ` or for our purposes `Float -> Float`. Also, notice the functions are not giving exact answers in some cases and instead are giving approximations. These functions are quite unnatural for a computer, so we surely know that the computer isn't processing them. So what is happening under the hood? Well, for trignometric functions, we will need to introduce some more stuff but we will circle back to it. As for the roots and logithrms, we can do a lot of it rather easily.

#def[
  Newton–Raphson method is a method to find the roots of a function. 
]



// cite
// Curry Howerd by Example - CJ Quines
// Programming in Haskell - Grahm Hutton
