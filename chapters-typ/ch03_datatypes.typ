#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise

#let definition = def
#let example = it => [For example - \ #it]

= Introduction to Types <intro-to-types>
Haskell is a strictly typed language. This means, Haskell needs to strictly know what the type of *anything* and everything is.

But one would ask here, what is type? According to Cambridge dictionary, 
#def[*Type* refers to a particular group of things that share similar characteristics and form a smaller division of a larger set.]

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

  `Integer` can hold any number no matter how big, up to the limit of your machine's memory, while `Int` corresponds to the set of positive and negative integers that can be expressed in 32 or 64 bits(based on system) with the bounds changing depending on implementation (guaranteed at least $-2^29$ to $2^29$). Going outside this range may give weird results. /*why not us #ex*/ Ex. /*Do we expect them to know `product at this point?`*/`product [1..52] :: Int` gives a negative number which cannot realistically be $52!$. On the other hand, `product [1..52] :: Integer` gives indeed the correct answer.

  The reason for `Int` existing despite its bounds and us not using `Integer` for everything is related to speed and memory. Using the former is faster and uses lesser memory.

  ```
  >>> product [1..52] :: Int
  -8452693550620999680
  (0.02 secs, 87,896 bytes)
  >>> product [1..52] :: Integer
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
  >>> sqrt 2 :: Float
  1.4142135
  >>> sqrt 99999 :: Float
  316.2262
  >>> sqrt 2 :: Double
  1.4142135623730951
  >>> sqrt 99999 :: Double
  316.226184874055
  >>> sqrt 999999999 :: Double
  31622.776585872405
  ```
We can see that the prescission of $sqrt(99999)$ is much lower than that of $sqrt(2)$. We will use `Float` for most of this book. //please no
]

#def[
  A tuple is a length immutable(can't be changed), ordered multi-typed data structure. This means we can store a fixed number of multiple types of data in an order using tuples. Ex.
  `(False , True ) :: (Bool, Bool)`
  `(False , 'a', True ) :: (Bool, Char, Bool)` 
  `("Yes", 5.21 , 'a') :: (String, Float, Char)`
  
  A list is a length mutable(can be changed), ordered, single typed data structure. This means we can store an arbitrary number things of the same type in a certain order using lists. Ex.
  `[False, True, False] :: [Bool]`
  `['a','b','c','d'] :: [Char]`
  `["One","Two","Three"] :: [String]`
]
Chapter 4 deals with tuples in more details and Chpater 5 and 7 with lists.

#def[
  *`Char`* are the types used to represent arbitrary Unicode characters. This includes all numbers, letters, white spaces(space, tab, newline etc) and other special characters.

  *`String`* is the type used to represent a bunch of characters chained together. Every word, sentence, paragraph is either a string or a collection of them.

  In haskell, Strings and Chars are differentiated using the type of quotation used. `"hello" :: String` as well as `"H" :: String` but `'H':: Char`. Unlike some other languages, like say Python, we can't do so interchangeably. Double Quotes for Strings and Single Quotes for Chars.
]
Similer to many modern languages, In Haskell, String is just a synonym for a list of characters that is `String` is same as `[Char]`. This allows string manipulation to be extremely easy in Haskell and is one of the reason why Pandoc, a universal document converter and one of the most used software in the world, is written in Haskell. You will see some more details on this in chapter 5.

= Logical Operations
#def(sub : "Logical Operators")[
  Logical operators are functions used to combine or modify Boolean values according to a set of rules.

  You have probably already seen some of them in either math or physics.
]
#exercise(sub:"Logical Operators")[
  Write Haskell code to simulate the following logical operators
  + NOT
  + OR
  + AND
  + NAND
  + XOR
]
Implementing a `not` operator seems the most straightforward and it indeed is. We can simply specify the output for all the cases, as there are only $2$.
```
-- | Definition of Not
not :: Bool -> Bool
not True = False
not False = True
```
The inbuilt function is also called `not`. When used in math, `not` is reprasented usign $not$ or $tilde$ before the variable, ie $not A$ or $tilde A$.

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
-- | Definition of Or
or :: Bool -> Bool -> Bool
or a b = if (a,b) == (False, False) then False else True
```
The inbuilt operator for this is `||` used as `False || True` which evaluates to `True`. 

In math, we reprasent `or` using $or$ or $+$ ie. $A or B$ or $A + B$. Note, this meaning for $+$ is only taken when $A, B$ are booleans.

How would one write such a code for `and`? This is left as exercise for the reader. 

The inbuilt operator for this is `&&` used as `True && False` which evaluates to `False`. The mathematical equivalen is $and$ or $dot$ ie. $A and B$ or $A dot B$. Again, note that this meaning for $dot$ is only tsken when $A,B$ are booleans.

Now that we already have `and` and `not`, could we make `nand` by just composing them? Sure!
```
-- | Definition of Nand
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)
```
This also seems like as good of a time as any to introduce operation conversion and function composition. 

In Haskell, functions are first class citizens. It is a functional programming language after all. Given two functions, we naturally want to compose them. Say we want to make the function $h(x) : x |-> -x^2$ and we have $g(x) : x |-> x^2$ and $f(x) : x |-> -x$. So we can define $h(x) := (f compose  g)(x) = f(g(x))$. In haskell, this would look like
```
negate :: Int -> Int
negate x = - x

square :: Int -> Int
square x = x^2

negateSquare :: Int -> Int
negateSquare x = negate . square
```
We could also define `negateSquare` in a more cumbersome `negateSquare x = negate(square x)` but with complicated expressions these brackets will add up and we want to avoid them as far as possible.

We will also now talk about the fact that the infix operators, like `+, -, *, /, ^, &&, ||` etc are also deep inside functions. This means we can should be able to access them as functions(to maybe compose them) as well as make our own. And we indeed can, the method is brackets and backticks.

*An operator inside a bracket is a function and a function in backticks is an operator.* For example
```
>>> True && False
False
>>> (&&) True False
False
>>> f x y = x*y + x + y
>>> f 3 4
19
>>> 3 `f` 4
19
```
Thus, as Haskell doesn't have an inbuilt nand operator, say I want to have `@@` to represent it. Then, I could write
```
(@@) :: Bool -> Bool -> Bool
(@@) a b = not (a && b)
```
Finally, we need to make `xor`.  We will now replicate a classic example of 17 ways to define it and a quick reference for a lot of the syntax. 
```
-- | 17 Xors
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
-- | 17 Xors contd.
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
-- | 17 Xors, contd.
xor10, xor11 :: Bool -> Bool -> Bool

xor10 b1 b2 = if b1 == b2 then False else True

xor11 b1 b2 = if b1 /= b2 then True else False
```

Or use the guard syntax. Similar to piecewise functions in math, we can define the function piecewise with the input changing the definition of the function, we can define guarded definition where the inputs control which definition we access. If the pattern(a condition) to a guard is met, that definition is accessed in order of declaration.

We do this as follows

```
-- | 17 Xors, cotd
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
Finally, we can define use the `case .. of ..` syntax. While this syntax is rarer, and too verbose, for simple functions, we will see a lot of it later in chapter 13, when we will talk about the monstrosity called monads. In this syntax, the general form is
```
case <expression> of
  <pattern1> -> <result1>
  <pattern2> -> <result2>
  ...
```
The case expression evaluates the `<expression>`, and matches it against each pattern in order. The first matching pattern's corresponding result is returned. You can nest case expressions to match on multiple values, although it can become extreamly unreadable, rather quickly.

```
-- | 17 Xors, contd
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

#exercise(sub : "Only Nand")[
  It is a well know fact that one can define all logical operators using only `nand`. Well, let's do so. 
  
  Redefine `and`, `or`, `not` and `xor` using only `nand`.
]

= Numerical Functions
A lot of numeric operators and functions come predefined in Haskell. Some natural ones are
```
>>> 7 + 3
10
>>> 3 + 8
11
>>> 97 + 32
129
>>> 3 - 7
-4
>>> 5 - (-6)
11
>>> 546 - 312
234
>>> 7 * 3
21
>>> 8*4
32
>>> 45 * 97
4365
>>> 45 * (-12)
-540
>>> (-12) * (-11)
132
>>> abs 10
10
>>> abs (-10)
10
```
The internal definition of addition and subtraction is discussed in the appendix while we talk about some multiplication algorithms in chapter 10. For now, assume that these functions work exactly as expected.

`Abs` is also implemented in a very simple fashion.
```
-- | Implementation of abs function
abs :: Num a => a -> a
abs a = if a >= 0 then a else -a
```

== Division, A Trilogy
Now let's move to the more interesting operators and functions. 

`recip` is a function which reciptocates a given numebr, but it has rather interesting type signature. It is only defined on types with the `Fractional` typeclass. /*suggest that we not mention typeclass yet*/ This refers to a lot of things, but the most common ones are `Rational, Float` and `Double`.  `recip`, as the name suggests, returns the reciprocal of the number taken as input. The type signature is `recip :: Fractional a => a -> a`
```
>>> recip 5
0.2
>>> k = 5 :: Int
>>> recip k
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
>>> 100 `div` 7
14
>>> 100 `mod` 7
2
>>> 100 `divMod` 7
(14,2)
>>> 100 `quot` 7
14
>>> 100 `rem` 7
2
>>> 100 `quotRem` 7
(14,2)
```
One must wonder here that why would we have two functions doing the same thing? Well, they don't actually do the same thing.

#exercise(sub : "Div vs Quot")[
  From the given example, what is the difference between `div` and `quot`?
  ```
>>> 8 `div` 3
2
>>> (-8) `div` 3
-3
>>> (-8) `div` (-3)
2
>>> 8 `div` (-3)
-3
>>> 8 `quot` 3
2
>>> (-8) `quot` 3
-2
>>> (-8) `quot` (-3)
2
>>> 8 `quot` (-3)
-2
```
]

#exercise(sub : "Mod vs Rem")[
  From the given example, what is the difference between `mod` and `rem`?
  ```
>>> 8 `mod` 3
2
>>> (-8) `mod` 3
1
>>> (-8) `mod` (-3)
-2
>>> 8 `mod` (-3)
-1
>>> 8 `rem` 3
2
>>> (-8) `rem` 3
-2
>>> (-8) `rem` (-3)
-2
>>> 8 `rem` (-3)
2
```
]

While the functions work similerly when the divisior and dividend are of the same sign, they seem to diverge when the signs don't match.

The thing here is we always want our division algorithm to satisfy $d * q + r = n, |r| < |d|$ where $d$ is the divisior, $n$ the dividend, $q$ the quotient and $r$ the remainder. 

The issue is for any $- d < r < 0 => 0 < r < d$. This means we need to choose the sign for the remainder. 

In Haskell, `mod` takes the sign of the divisor (comes from floored division, same as Python's `%`), while `rem` takes the sign of the dividend (comes from truncated division, behaves the same way as Scheme's `remainder` or C's `%`.).

Basically, `div` returns the floor of the true divison value (recall $floor(-3.56) = -4$) while `quot` returns the trunicated value of the true division (recall $op("trunicate")(-3.56) = -3$ as we are just trunicating the decimal point off). The reason we keep both of them in Haskell is to be comfertable for people who come from either of these languages. 

Also, The `div` function is often the more natural one to use, whereas the `quot` function corresponds to the machine instruction on modern machines, so it's somewhat more efficient (although not much, I had to go upto $10^100000$ to even get millisecond difference in the two).

A simple excercise for us now would be implementing our very own integer division algorithm. We begin with a division algorithm for only positive integers.
```
-- | A division algorithm on positive integers by repreated subtraction
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
  | otherwise = divide n d

divide :: Integer -> Integer -> (Integer, Integer)
divide n d = go 0 n where
  go q r = if r >= d then go (q+1) (r-d) else (q,r)
```
#exercise(sub : "Another Division")[
  Figure out which kind of division have we implemented above, floored or trunicated.

  Now implement the other one yourself by modifing the above code appropriatly.
]

== Exponentiation
Haskell defines for us three exponation operators, namely `(^^), (^), (**)`. 

#exercise(sub : "Can you see the difference?")[
  What can we say about the three exponation operators?
  ```
  >>> a = 5 :: Int
  >>> b = 0.5 :: Float
  >>>
  >>> a^a
  3125
  >>> a^^a
  <interactive>:4:2: error: [GHC-39999]
  >>> a**a
  <interactive>:5:2: error: [GHC-39999]
  >>>
  >>> a^b
  <interactive>:6:2: error: [GHC-39999]
  >>> a^^b
  <interactive>:7:2: error: [GHC-39999]
  >>> a**b
  <interactive>:8:4: error: [GHC-83865]
  >>>
  >>> b^a
  3.125e-2
  >>> b^^a
  3.125e-2
  >>> b**a
  <interactive>:11:4: error: [GHC-83865]
  >>>
  >>> b^b
  <interactive>:12:2: error: [GHC-39999]
  >>> b^^b
  <interactive>:13:2: error: [GHC-39999]
  >>> b**b
  0.70710677
  >>>
  >>> a^(-a)
  *** Exception: Negative exponent
  >>> a^^(-a)
  <interactive>:16:2: error: [GHC-39999]
  >>> a**(-a)
  <interactive>:17:2: error: [GHC-39999]
  >>>
  >>> b^(-a)
  *** Exception: Negative exponent
  >>> b^^(-a)
  32.0
  >>> b**(-a)
  <interactive>:20:6: error: [GHC-83865]
  ```
]

Unlike division, they have almost the same function. The difference here is in the type signature. While, infering the exact type signature was not expected, we can notice:
- `^` is raising genral numbers to positive integral powers. This means it makes no assumptions about if the base can be reciprocated and just produces an exception if the power is negative and error if the power is fractional.
- `^^` is raising fractional numbers to general integral powers. That is, it needs to be sure that the reciprocal of the base exists (negative powers) and doesn't throw an error if the power is negative.
- `**` is raising numbers with floating point to powers with floating point. This makes it the most general exponation.

The operators clearly get more and more general as we go down the list but they also get slower. However, they are also reducing in accurecy and may even output `Infinity` in some cases. The `...` means I am trunicating the output for readablity, ghci did give the compelete answer.

```
>>> 2^1000
10715086071862673209484250490600018105614048117055336074...
>>> 2 ^^ 1000
1.0715086071862673e301
>>> 2^10000
199506311688075838488374216268358508382...
>>> 2^^10000
Infinity
>>> 2 ** 10000
Infinity
```

The exact reasons for the inaccuracy comes from float conversions and approximation methods. We will talk very little about this specialist topic somewhat later. 

However, something within our scope is implementing `(^)` ourselves.

```
-- | A naive integer exponation algorithm
exponation :: (Num a, Integral b) => a -> b -> a
exponation a 0 = 1
exponation a b = if b < 0 
  then error "no negitve exponation" 
  else a * (exponation a (b-1))
```
This algorithm, while the most naive way to do so, computes $2^100000$ in mearly $0.56$ seconds.

However, we could do a bit better here. Notice, to evaluate $a^b$, we are making $b$ multiplications. 

A fact, which we shall prove in chapter 10, is that multiplication of big numbers is faster when it is balenced, that is the numbers being multiplied have similer number of digits.

So to do better, we could simply compute $a^(b/2)$ and then square it, given $b$ is even, or compute $a^((b-1)/2)$ and then square it and multiply by $a$ otherwise. This can be done recusrsivly till we have the solution. 
```
-- | A better exponentiation algorithm using divide and conquer
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

The idea is that we are now making atmost $3$ multiplications at each step and there are atmost $ceil(log_2(b))$ steps. This brings us down from $b$ multiplications to $3 log(b)$ multiplications. Furthermore, most of these multiplications are somewhat balenced and hence optimized.

This kind of a stratergy is called divide and conquer. You take a big problem, slice it in half, solve the smaller version, and then stitch the results together. It’s a method/technique that appears a lot in Computer Science (in sorting, in searching through data, in even solving diffrential equations and training AI models) and we will see it again shortly.

Finally, there’s one more minor optimization that’s worth pointing out. It's a small thing, and doesn't even help that much in this case, but if the multiplication were particularly costly, say as in matrices; our exponation method could be made slightlty better. Let's say we are dealing with say $2^31$. Our current algorithm would evaluate it as:
$
2^31 
= (2^15)^2 dot 2\
= ((2^7)^2 dot 2)^2 dot 2\
= (((2^3)^2 dot 2)^2 dot 2)^2 dot 2\
= ((((2^1)^2 dot 2)^2 dot 2)^2 dot 2)^2 dot 2\
$
This is a problem as the small $dot 2$ in every bracket are unbalenced. The exact way we deal with all this is by something called #link("en.wikipedia.org/wiki/Exponentiation_by_squaring#2k-ary_method")[2^k arry method]. Although, more often then not, most built in implementations use the divide and conquer exponentiation we decribed.

== `gcd` and `lcm`
A very common function for number theoretic use cases is `gcd` and `lcm`. They are pre-defined as
```
>>> :t gcd
gcd :: Integral a => a -> a -> a
>>> :t lcm
lcm :: Integral a => a -> a -> a
>>> gcd 12 30
6
>>> lcm 12 30
60
```
We will now try to define these functions ourselves.

A naive way to do so would be:
```
-- | Naive GCD and LCM
-- Uses a brute-force approach starting from the smaller number and counting down
gcdNaive :: Integer -> Integer -> Integer
gcdNaive a 0 = a
gcdNaive a b = 
    if b > a 
        then gcdNaive b a  -- Ensure first argument is greater
        else go a b b
  where
    -- Start checking from the smaller of the two numbers
    go x y current =
        if (x `mod` current == 0) && (y `mod` current == 0)
            then current
            else go x y (current - 1)

-- Uses a brute-force approach starting from the larger number and counting up
lcmNaive :: Integer -> Integer -> Integer
lcmNaive a b = 
    if b > a 
        then lcmNaive b a  -- Ensure first argument is greater
        else go a b a
  where
    -- Start checking from the larger of the two numbers
    go x y current =
        if current `mod` y == 0
            then current
            else go x y (current + x)
```

These both are quite slow for most practical uses. A lot of cryptography runs on computer's ability to find gcd and lcm fast enough. If this was the fastest, we would be cooked. So what do we do? Call some math.

A simple optimization could be using $p dot q = gcd(p,q) dot lcm(p,q)$. This makes the speed of both the operations same, as once we have one, we almost already have the other.

Let's say we want to find $g := gcd(p,q)$ and $p > q$. That would imply $p = d q + r$ for some $r < q$. This means $g | p, q => g | q, r$ and by the maximality of $g$, $gcd(p,q) = gcd(q,r)$. This helps us out a lot as we could eventually reduce our problem to a case where the larger term is a multiple of the smaller one and we could return the smaller term then and there. This can be implemented as:
```
-- | Fast GCD and LCM
gcdFast :: Integer -> Integer -> Integer
gcdFast p 0 = p -- Using the fact that the moment we get q | p, we will reduce to this case and output the answer.
gcdFast p q = gcdFast q (p `mod` q)


lcmFast :: Integer -> Integer -> Integer
lcmFast p q = (p * q) `div` (gcdFast p q)
```

We can see that this is much faster. The exact number of steps or time taken is a slightlty involved and not very related to what we cover. Intrested readers may find it and related citrations #link("https://en.wikipedia.org/wiki/Euclidean_algorithm#Algorithmic_efficiency")[here].

This algorithm predates computers by approximatly 2300 years. If was first decribed by Euclid and hence is called the Euclidean Algorithm. While, faster algorithms do exist, the ease of implementation and the fact that the optimizations are not very dramatic in speeding it up make Euclid the most commonly used algorithm.

While we will see these class of algorithms, including checking if a number is prime or finding the prime factorization, these require some more weapons of attack we are yet to devlop.

== Recursive Functions
A lot of mathematical functions are defined recusrsivly. We have already seen a lot of them in chapter 1 and excercises. Factorial, binomials and fibbonacci are common examples.
```
-- | Factorial and Binomial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n *  factorial (n-1)

nCr :: Integer -> Integer -> Integer
nCr _ 0 = 1
nCr n r 
  |r > n      = 0
  |n == r     = 1
  |otherwise  = (nCr (n-1) (r-1)) + (ncr (n-1) r)
```
For fibbonacci, note that the naive definition, is well naive.
```
-- | naive fibbonacci defination
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```
This is becuase we keep recomputing the same values again and again. For example computing `fib 5` according to this scheme would look like:
```
fib 5 = fib 4 + fib 3
      = (fib 3 + fib 2) + (fib 2 + fib 1)
      = ((fib 2 + fib 1) + (fib 1 + fib 0)) + ((fib 1 + fib 0) + 1)
      = (((fib 1 + fib 0) + 1) + (1 + 1)) + ((1 + 1) + 1)
      = (((1 + 1) + 1) + (1 + 1)) + ((1 + 1) + 1)
      = 8
```
We can use less operations by simply storing some of the values, in this case just having the last one handy will be enough.
```
-- | Fibonacci with Tail recursion
fibbonacci :: Integer -> Integer
fibbonacci n = go n 1 1 where
  go 0 a _ = a
  go n a b = go (n - 1) b (a + b)
```
We can see that this is much more efficient. Tracing the computation of `fibbonacci 5` now looks like:
```
fibbonacci 5 
= go 5 1 1
= go 4 1 2
= go 3 2 3
= go 2 3 5
= go 1 5 8
= go 0 8 13
= 8
```
This is called tail recursion as we carry the tail of the recursion to speed things up. It can be used to speed up naive recursion, although not always.

Another way to evaluate fibbonacci will be seen in end of chapter excercises, where we will translate Binet's formula straight into Haskell. Why can't we do so directly? As we can't reprasent $sqrt(5)$ exactly and the small errors in the approximation will accumalate due to the number of operations. This excercise should allow you to end up with a blazzingly fast algorithm which can compute the $12.5$-th million fibbonacci number in 1 sec. Our tail recursive formula takes more than 2 mins to reach there.

= Mathematical Functions
We will now talk about mathematical functions like `log`, `sqrt`, `sin`, `asin` etc. We will also take this oppurtunity to talk about real exponation. To begin, Haskell has a lot of pre-defined functions.

```
>>> sqrt 81
9.0

>>> log (2.71818)
0.9999625387017254
>>> log 4
1.3862943611198906
>>> log 100
4.605170185988092
>>> logBase 10 100
2.0
>>> exp 1
2.718281828459045
>>> exp 10
22026.465794806718

>>> pi
3.141592653589793
>>> sin pi
1.2246467991473532e-16
>>> cos pi
-1.0
>>> tan pi
-1.2246467991473532e-16
>>> asin 1
1.5707963267948966
>>> asin 1/2
0.7853981633974483
>>> acos 1
0.0
>>> atan 1
0.7853981633974483
```
`pi` is a predefined variable inside haskell. It carries the value of $pi$ upto some decimal places based on what type it is forced in.
```
>>> a = pi :: Float
>>> a
3.1415927
>>> b = pi :: Double
>>> b
3.141592653589793
```
All the fucntions above have the type signature `Fractional a => a -> a ` or for our purposes `Float -> Float`. Also, notice the functions are not giving exact answers in some cases and instead are giving approximations. These functions are quite unnatural for a computer, so we surely know that the computer isn't processing them. So what is happening under the hood?

== Binery Search

Recall the question Caves and Carnivel in chapter 1. How did you solve them?

#def(sub : "Hi-Lo game or Bisection Search")[
  Imagine you are playing a number guessing game with a friend.
  
  Your friend is thinking of a number between $1$ and $100$, and you have to guess it.
  
  After every guess, your friend will say whether your guess is too high, too low, or correct.

  So what do you do to win as fast as possible? 
  
  You don’t start at 1. You start at 50. Why? Because 50 cuts the range exactly in half. Depending on whether the answer is higher or lower, you can now ignore half the numbers.
  
  Next guess? Halfway through the remaining half. Then half of that. And so on.
  
  This is called a binary search or a bisection search: each step cuts the list in half, so you zoom in on the answer quickly.
  
  Here’s how it works:
  
  - Start in the middle of a some ordered list.
  - If the middle item is your target, you’re done.
  - If it’s too big, repeat the search on the left half.
  - If it’s too small, repeat on the right half.

In practice, we keep halving till we find it; or realize it’s not there.
]

While using a raw binery search for roots would be impossible as the exact answer is seldom rational and hence, the algorithm would never terminate. So instead of searching for the exact root, we look for an approximation by keeping some tolerence. Here is what it looks like:
```
-- | Square root by binary search
bsSqrt :: Float -> Float -> Float
bsSqrt n tolerance
  | n > 1     = binarySearch 1 n
  | otherwise = binarySearch 0 1
  where
    binarySearch low high
      | abs (guess * guess - n) <= tolerance        = guess
      | guess * guess > n                           = binarySearch low guess
      | otherwise                                   = binarySearch guess high
      where
        guess = (low + high) / 2
```
#exercise(sub : "Cube Root")[
  Write a function `bsCbrt :: Float -> Float -> Float` which calculates the cube root of a number upto some tolerance using binery search.
]

The internal implementation sets the tolerance to some constant, defining, for example as `sqrt = bsSqrt 0.00001 `

Furthermore, there is a faster method to compute square roots and cube roots(in general roots of polynomials), which uses a bit of analysis. You will find it defined and walked-through in the back excercise.

Finally, as we are on the topic of binary search, we will also explore some other used of the same. One of it is basically, binary searching the answer to a question.

#exercise(sub : "Hamburgers (Codeforces)")[
Polycarpus have a fixed hamburger recipe using $B$ pieces of bread , $S$ pieces of sausage and $C$ pieces of cheese; per burger.

At the current moment, in his pantry he has:

- $n_b$ units of bread,
- $n_s$ units of sausage,
- $n_c$ units of cheese.

And the market prices per unit is:

- $p_b$ rubles per bread,
- $p_s$ rubles per sausage,
- $p_c$ rubles per cheese.

Polycarpus's wallet has $r$ rubles.

Each hamburger must be made exactly according to the recipe (ingredients cannot be split or substituted), and the store has an unlimited supply of each ingredient.

Write function `burgers :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Int -> Int` which takes $(B,S,C)$, $(n_b, n_s, n_c)$, $(p_b, p_s, p_c)$ and $r$ and tells us how many burgers can Polycarpus make. 

Examples
```
burgers (3,2,1) (6,4,1) (1,2,3) 4 = 2
burgers (2,0,1) (1,10,1) (1,10,1) 21 = 7
burgers (1,1,1) (1,1,1) (1,1,3) 1000000000000 = 200000000001
```
]

This question may look like a combinitorics or recursion question, but any of thoose approaches will be very inefficient. 

Let's try to algerbrically compute how muh money is needed to make $x$ burgers. We can define this cost function as cost times the number of ingredient required minus the amount already in pantry. This will something like:
$
  f(x) = p_b max(0, x dot B - n_b) + p_s max(0, x dot S - n_s) + p_c max(0, x dot C - n_c)
$
And now we want to look for maximal $x$ such that $f(x) <= r$. Well, that can be done using Binery search!
```
burgers (b, s, c) (nb, ns, nc) (pb, ps, pc) r = binarySearch 0 upperBound
  where
    -- Cost function f(x)
    cost x = let needB = max 0 (x * b - nb)
                 needS = max 0 (x * s - ns)
                 needC = max 0 (x * c - nc)
             in  needB *  pb +
                 needS *  ps +
                 needC *  pc

    upperBound = maximum [b,s,c] + r

    binarySearch low high
      | low > high = high
      | otherwise  =
          let mid = (low + high) `div` 2
          in if cost mid <= r
                then binarySearch (mid + 1) high
                else binarySearch low (mid - 1)
```
Here ia a similar excercise for your practice.

#exercise(sub : "House of Cards (Codeforces)")[
- A house of cards consists of some non-zero number of floors.
- Each floor consists of a non-zero number of rooms and the ceiling. A room is two cards that are leaned towards each other. The rooms are made in a row, each two adjoining rooms share a ceiling made by another card.
- Each floor besides for the lowest one should contain less rooms than the floor below.

Please note that the house may end by the floor with more than one room, and in this case they also must be covered by the ceiling. Also, the number of rooms on the adjoining floors doesn't have to differ by one, the difference may be more.

The height of the house is the number of floors in it. 

Given $n$ cards, it is possible that you can make a lot of different houses of different heights. Write a function `houses :: Integer -> Integer` to count the number of the distinct heights of the houses that they can make using exactly $n$ cards.

Examples
```
count 13 = 1
count 6 = 0
```

In the first sample you can build only these two houses (remember, you must use all the cards):

#image("../images/cardHouses.png")

Thus, 13 cards are enough only for two floor houses, so the answer is 1.

The six cards in the second sample are not enough to build any house.
]

== Taylor Series

However, the method of binery search won't work for `log` as we would need to do real exponation, which, as we will soon see, is defined using `log`. So what do we do? Taylor series and reduction.

We know that $ln(1+x) = x - x^2/2 + x^3/3 - dots$. For small $x, ln(1+x) approx x$. So if we can create a scheme to make $x$ small enough, we could get the logithrm by simply multiplying. Well, $ln(x^2) = 2 ln(|x|)$. So, we could simply keep taking square roots of a number till it is within some error range of $1$ and then simply use the fact $ln(1+x) approx x$ for small $x$.
```
-- | Log defined using Taylor Approximation
logTay :: Float -> Float -> Float
logTay tol n 
  | n <= 0                      = error "Negative log not defined"
  | abs(n - 1) <= tol           = n - 1  -- using log(1 + x) ≈ x
  | otherwise                   = 2 * logTay tol (sqrt n)
```
This is a very efficient algorithm for approximating `log`. Doing better requires the use of either pre-computed lookup tables(which would make the programme heavier) or use more sophesticated mathematical methods which while more accurate would slow the programme down. There is an excercise in the back, where you will implement a state of the art algorithm to compute `log` accurately upto 400-1000 decimal places.

Finally, now that we have `log = logTay 0.0001`, we can easily define some other functions.
```
logBase a b = log(b) / log(a)
exp n = if n == 1 then 2.71828 else (exp 1) ** n
(**) a b = exp (b * log(a))
```

We will use this same Taylor approximation scheme for `sin` and `cos`. The idea here is: $sin(x) approx x$ for small $x$ and $cos(x) = 1$ for small $x$. Furthermore, $sin(x+2pi) = sin(x)$, $cos(x + 2 pi) = cos(x)$ and $sin(2x) = 2 sin(x) cos(x)$ as well as $cos(2x) = cos^2(x) - sin^2(x)$.

This can be encoded as
```
-- | Sin and Cos using Taylor Approximation
sinTay :: Float -> Float -> Float
sinTay tol x
  | abs(x) <= tol        = x  -- Base case: sin(x) ≈ x when x is small
  | abs(x) >= 2 * pi     = if x > 0 
                            then sinTay tol (x - 2 * pi) 
                            else sinTay tol (x + 2 * pi)  -- Reduce x to [-2π, 2π]
  | otherwise            = 2 * (sinTay tol (x/2)) * (cosTay tol (x/2))  -- sin(x) = 2 sin(x/2) cos(x/2)

cosTay :: Float -> Float -> Float
cosTay tol x
  | abs(x) <= tol        = 1.0  -- Base case: cos(x) ≈ 1 when x is small
  | abs(x) >= 2 * pi     = if x > 0 
                            then cosTay tol (x - 2 * pi) 
                            else cosTay tol (x + 2 * pi)  -- Reduce x to [-2π, 2π]
  | otherwise            = (cosTay tol (x/2))**2 - (sinTay tol (x/2))**2  -- cos(x) = cos²(x/2) - sin²(x/2)
```

As one might notice, this approximation is somewhat poorer in accuracy than `log`. This is due to the fact that the taylor approximation is much less truer on `sin` and `cos` in the neighbourhood of `0` than for `log`. 

We will see a better approximation once we start using lists, using the power of the full Taylor expansion. 

Finally, similer to our above things, we could simply set the tolerance and get a function that takes an input and gives an output, name it `sin` and `cos` and define `tan x = (sin x) / (cos x)`.

#exercise(sub : "Inverse Trig")[
Use taylor approximation and trignometric identites to define inverse sin(`asin`), inverse cos(`acos`) and inverse tan(`atan`).
]

= Excercise
#exercise(sub: "Collatz")[
  Collatz conjucture states that for any $n in NN$ exists a $k$ such that  $c^k(n) = 1$ where $c$ is the Collatz function which is $n/2$ for even $n$ and $3n + 1$ for odd $n$.

  Write a functuon `col :: Integer -> Integer` which, given a $n$, finds the smalltest $k$  such that $c^k(n) = 1$, called the Collatz chain length of $n$.
]

#exercise(sub: "Newton–Raphson method")[
#def(sub: "Newton–Raphson method")[
  Newton–Raphson method is a method to find the roots of a function via subsequent approximations.
  
  Given $f(x)$, we let $x_0$ be an inital guess. Then we get subsequent guesses using
  $
    x_(n+1) = x_n - f(x_n)/(f'(x_n))
  $
  As $n -> oo$, $f(x_n) -> 0$.

  The intution for why this works is: imagine standing on a curve and wanting to know where it hits the x-axis. You draw the tangent line at your 
  current location and walk down it to where it intersects the x-axis. That’s your next guess. Repeat. If the curve behaves nicely, you converge quickly to the root.
  
  Limitations of Newton–Raphson method are
  - Requires derivative: The method needs the function to be differentiable and requires evaluation of the derivative at each step.
  - Initial guess matters: A poor starting point can lead to divergence or convergence to the wrong root.
  - Fails near inflection points or flat slopes: If $f'(x)$ is zero or near zero, the method can behave erratically.
  - Not guaranteed to converge: Particularly for functions with multiple roots or discontinuities.
]
Considering, $f(x) = x^2 - a$ and $f(x) = x^3 - a$ are well behaved for all $a$, implement `sqrtNR :: Float -> Float -> Float` and `cbrtNR :: Float -> Float -> Float` which finds the square root and cube root of a number upto a tolerance using the Newton–Raphson method.

Hint: The number we are trying to get the root of is a sufficiently good guess for numbers absolutly greater than $1$. Otherwise, $1$ or $-1$ is a good guess. We leave it to your mathematical intution to figure out when to use what.
]

#exercise(sub:"Digital Root")[
  The digital root of a number is the digit obtained by summing digits until you get a single digit. For example `digitalRoot 9875 = digitalRoot (9+8+7+5) = digitalRoot 29 = digitalRoot (2+9) = digitalRoot 11 = digitalRoot (1+1) = 2`. 
  
  Implement the function `digitalRoot :: Int -> Int`.
  ]
#exercise(sub: "AGM Log")[
  A rather uncommon mathematical function is AGM or arthmatic-geometric mean. For given two numbers, 
  $
  op("AGM")(x,y) = cases(
    x & text("if") x = y\
    op("AGM") ((x+y)/2, sqrt(x y)) & text("otherwise")
  )
  $
  Write a function `agm :: (Float, Float) -> Float -> Float` which takes two floats and returns the AGM within some tolerance(as getting to the exact one recusrsivly takes, about infinite steps).

  Using AGM, we can define
  $
    ln(x) approx pi/(2 op("AGM")(1, 2^(2-m)/x)) - m ln(2)
  $
  which is precise upto $p$ bits where $x 2^m > 2^(p/2)$.

  Using the above defined `agm` function, define `logAGM :: Int -> Float -> Float -> Float` which takes the number of bits of precision, the tolerance for `agm` and a number greater than $1$ and gives the natural logithrm of that number.

  Hint: To simplify the question, we added the fact that the input will be greater than $1$. This means a simplification is taking `m = p/2` directly. While geting a better `m` is not hard, this is just simpler.
]
#exercise(sub: "Multiplexer")[
  A multiplexer is a hardware element which chooses the input stream from a variety of streams.

  It is made up of $2^n + n$ components where the $2^n$ are the input streams and the $n$ are the selectors.

  (i) Implement a 2 stream multiplex `mux2 :: Bool -> Bool -> Bool -> Bool` where the first two booleans are the inputs of the streams and the third boolean is the selector. When the selector is `True`, take input from stream $1$, otherwise from stream $2$.

  (ii) Implement a 2 stream multiplex using only boolean operations.
  
  (iii) Implement a 4 stream multiplex. The type should be `mux4 :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool`. (There are 6 arguments to the function, 4 input streams and 2 selectors). We encourage you to do this in atleast 2 ways (a) Using boolean operations (b) Using only `mux2`.

  Could you describe the general scheme to define `mux2^n` (a) using only boolean operations (b) using only `mux2^(n-1)` (c) using only `mux2`?
]
#exercise(sub: "Moduler Exponation")[
  Implement modular exponentiation ($a^b mod m$) efficiently using the fast exponentiation method. The type signature should be `modExp :: Int -> Int -> Int -> Int`
]

#exercise(sub: "Bean Nim (Putnam 1995, B5)")[
  A game starts with four heaps of beans containing $a$, $b$, $c$, and $d$ beans. A move consists of taking either

(a) one bean from a heap, provided at least two beans are left behind in that
heap, or

(b) a complete heap of two or three beans.

The player who takes the last heap wins. Do you want to go first or second?

Write a recursive function to solve this by brute force. Call it `naiveBeans :: Int -> Int -> Int -> Int -> Bool` which gives `True` if it is better to go first and `False` otherwise. Play around with this and make some observations.

Now write a much more efficient (should be one line and has no recursion) function `smartBeans :: Int -> Int -> Int -> Int -> Bool` which does the same.
]

#exercise(sub : "Squares and Rectangles on a chess grid")[
  Write a function `squareCount :: Int -> Int` to count number of squares on a  $n times n$ grid. For example, `squareCount 1 = 1` and `squareCount 2 = 5` as four 1x1 squares and one 2x2 square.

  Furthermore, also make a function `rectCount :: Int -> Int` to count the number of rectangles on a $n times n$ grid.

  Finally, make `genSquareCount :: (Int, Int) -> Int` and `genRectCount :: (Int, Int) -> Int` to count number of squares and rectangle in a $a times b$ grid.
]

#exercise(sub:"Knitting Baltik (COMPFEST 13, Codeforces)")[
Mr. Chanek wants to knit a batik, a traditional cloth from Indonesia. The cloth forms a grid with size $m times n$. There are $k$
colors, and each cell in the grid can be one of the $k$ colors.

Define a sub-rectangle as an pair of two cells $((x_1,y_1), (x_2,y_2))$, denoting the top-left cell and bottom-right cell (inclusively) of a sub-rectangle in the grid.
Two sub-rectangles $((x_1,y_1), (x_2,y_2))$ and $((x_1,y_1), (x_2,y_2))$ have the same pattern if and only if the following holds:

(i) they have the same width ($x_2 - x_1 = x_4 - x_3$);

(ii) they have the same height ($y_2 - y_1 = y_4 - y_3$);

(iii) for every pair $i,j$ such that $0 <= i <= x_2 - x_1$ and $0 <= j <= y_2 - y_1$, the color of cells $(x_1 + i, y_1 + j)$ and $(x_3 + i, y_3 + j)$ is the same.

Write a function `countBaltik :: (Int, Int) -> Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Integer` to count the number of possible batik color combinations, such that the subrectangles $((a_x,a_y),(a_x+r-1,a_y + c - 1))$ and $((b_x,b_y),(b_x+r-1,b_y + c - 1))$ have the same pattern.

*Input*
`countBaltik` takes as input:

- The size of grid $(m, n)$
- Numer of colors $k$
- Size of sub-rectangle $(r,c)$
- $(a_x, a_y)$
- $(b_x,b_y)$

and should output an integer denoting the number of possible batik color combinations.

For example: `countBaltik (3,3) 2 (2,2) (1,1) (2,2) = 32`. The following are all 32 possible color combinations in the first example.
#image("../images/baltik.png")
]

#exercise(sub: "Modulo Inverse")[
  Given a prime modulus   $p > a$  , according to Euclidean Division $p = k a + r$ where 
$
k a + r equiv 0 mod p\
=> & k a equiv -r mod p\
=> & - r a^(-1) equiv k mod p\
=> & a^(-1) equiv - k r^(-1) mod p
$ 

Using this, implement `modInv :: Int -> Int -> Int` which takes in $a$ and $p$ and gives $a^(-1) mod p$.

Note that this reasoning does not hold if  $p$  is not prime, since the existence of  $a^(-1)$  does not imply the existence of  $r^(-1)$  in the general case.
]

#exercise(sub : "New Bakery(Codeforces)")[
  Bob decided to open a bakery. On the opening day, he baked $n$ buns that he can sell. The usual price of a bun is $a$ coins, but to attract customers, Bob organized the following promotion:

- Bob chooses some integer $k(0 <= k <= min(n,b))$.
- Bob sells the first $k$ buns at a modified price. In this case, the price of the $i$-th $(1 <= i <= k)$ sold bun is $(b-i+1)$ coins.
- The remaining $(n-k)$ buns are sold at $a$ coins each.

Note that $k$ can be equal to $0$. In this case, Bob will sell all the buns at $a$ coins each.

Write a function `profit :: Int -> Int -> Int -> Int` Help Bob determine the maximum profit he can obtain by selling all $n$ buns with $a$ being the normal price and $b$ the price of first bun to be sold at a modified price.

Example
```
profit          4         4           5 = 17
profit          5         5           9 = 35
profit         10        10           5 = 100
profit 1000000000 1000000000 1000000000 = 1000000000000000000
profit 1000000000 1000000000          1 = 1000000000000000000
profit       1000           1      1000 = 500500
```

Note

In the first test case, it is optimal for Bob to choose $k=1$. Then he will sell one bun for $5$ coins, and three buns at the usual price for $4$ coins each. Then the profit will be $5+4+4+4=17$ coins.

In the second test case, it is optimal for Bob to choose $k=5$. Then he will sell all the buns at the modified price and obtain a profit of $9+8+7+6+5=35$ coins.

In the third test case, it is optimal for Bob to choose $k=0$. Then he will sell all the buns at the usual price and obtain a profit of $10 dot 10=100$ coins.
]

#exercise(sub : "Sumac")[
A Sumac sequence starts with two non-zero integers $t_1$ and $t_2$.

The next term, $t_3=t_1−𝑡_2$

More generally, $t_𝑛=t_(𝑛−2)−t_(n-1)$

The sequence ends when $t_n≤0$. All values in the sequence must be positive.

Write a function `sumac :: Int -> Int -> Int` to compute the length of a Sumac sequence given the initial two terms, $t_1$ and $t_2$.

Examples(Sequence is included for clarification)
```
(t1,t2)   Sequence          n
------------------------------
(120,71)  [120,71,49,22,27] 5
(101,42)  [101,42,59]       3
(500,499) [500,499,1,498]   4
(387,1)   [387,1,386]       3
(3,-128)  [3]               1
(-2,3)    []                0
(3,2)     [3,2,1,1]         4
```
]

#exercise(sub: "Binet Formula")[
  Binet's formula is an explicit, closed form formula used to find the $n$th term of the Fibonacci sequence. It is so named because it was derived by mathematician Jacques Philippe Marie Binet, though it was already known by Abraham de Moivre.

  Thr problem with this remarkable formula is that it is clutered with irrational numbers, specifically $sqrt(5)$.
  $
    F_n = ((1+sqrt(5))^n - (1-sqrt(5))^n)/(2^n sqrt(5))
  $

  While computing using the binet formula would only take $2 * log(n) + 2$ operations (exponation takes $log(n)$ time), doing so directly is out of the question as we can't reprasent $sqrt(5)$ exactly and the small errors in the approximation will accumalate due to the number of operations.
  
  So an idea is to do all computations on a tuple $(a,b)$ which reprasents $a + b sqrt(5)$, We will need to define *addition, subtraction, multiplication and division* on these tuples as well as define a *fast exponation* here. 

  With that in hand, Write a function `fibMod :: Integer -> Integer` which computes Fibonacci numbers using this method.
]

#exercise(sub : "A puzzle (UVA 10025)")[
A classic puzzle involves replacing each $?$ with one can set operators $+$ or $-$, in order to obtain a given $k$
$
?1?2? dots ?n = k
$
For example: to obtain $k = 12$, the expression to be used will be:
$
- 1 + 2 + 3 + 4 + 5 + 6 - 7 = 12
$
with $n = 7$

Write function `puzzleCount :: Int -> Int` which given a $k$ tells us the smallest $n$ such that the puzzle can be solved.

Examples
```
puzzleCount 12 = 7
puzzleCount -3646397 = 2701
```
]

#exercise(sub : "Rating Recalculation (Code Forces)")[
It is well known in the Chess Federation that the boundary for the Grandmaster title is carefully maintained just above the rating of International Master Wupendra Wulkarni. However, due to a recent miscalculation in the federation’s new rating system, Wulkarni was mistakenly awarded the Grandmaster title.

To correct this issue, the federation has decided to revamp the division system, ensuring that Wupendra is placed into Division 2 (International Master), well below Grandmaster status.

A simple rule like `if rating <= wupendraRating then div = max div 2` would be too obvious and controversial. Instead, the head of the system, Mike, proposes a more subtle and mathematically elegant solution.

First, Mike chooses the integer parameter $k >= 0$. 

Then, he calculates the value of the function $f(r-k, r)$, where $r$ is the user's rating, and 
$
f(n,x) := (1+x+x^2/2! + x^3/3! + dots + x^n/n!)/(e^x)
$

Finally, the user's division is defined by the formula 
$
op("div")(r) = floor(1/f(r-k,r)) − 1
$.

Write function `ratingCon :: Int -> Int` to find the minimum $k$, given Wupendra's rating, so that the described algorithm assigns him a division strictly greater than $1$ and GM Wulkarni doesn't become a reality.

Examples
```
ratingCon 5 = 2
ratingCon 100 = 5
ratingCon 200 = 7
ratingCon 2500 = 23
ratingCon 3000 = 25
ratingCon 3500 = 27
```
]

#exercise(sub : "Knuth's Arrow")[
  Knuth introduced the following notation for a family of math notation:
  $
  3 dot 4 = 12\
  3 arrow.t 4 = 3 dot (3 dot (3 dot 3)) = 3^4 = 81\
  3 arrow.t arrow.t 4 = 3 arrow.t (3 arrow.t (3 arrow.t 3)) = 3^(3^3^(3)) = 3^7625597484987\ 
  approx 1.25801429062749131786039069820328121551804671431659601518967 × 10^3638334640024 \
  3 arrow.t arrow.t arrow.t 4 = 3 arrow.t arrow.t (3 arrow.t arrow.t (3 arrow.t arrow.t 3))
  $

  You can see the pattern as well as the extreame growth rate. Make a function `knuthArrow :: Integer -> Int -> Integer -> Integer` which takes the first argument, number of arrows and second argument and provides the answer.
]



// Suggest that we add a section about types and correctness of code.

// cite
// Curry Howerd by Example - CJ Quines
// Programming in Haskell - Grahm Hutton



// Editing Advice
// 1. Keep it relavent
// 2. Idiotproof
// 3. Define Stuff also Translate
// 4. Float?

//  Just a rando edit