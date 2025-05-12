#import "style.typ": *
#show: codly-init.with()
#codly(languages: codly-languages)

= Bool, Int, Integer and more (feel free to change it)

== Introduction to Types
Haskell is a strictly typed language. This means, Haskell needs to strictly know what the type of *anything* and everything is.

But one would ask here, what is type? According to Cambridge dictionary, 
#definition[*Type* is refers to a particular group of things that share similar characteristics and form a smaller division of a larger set]

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
- The type of $x |-> 2*x$ is *`Int -> Int`* when it takes integers to integers. It can also be *`Real -> Real`* when it takes reals to reals. These are two different functions, because they have different types. But if we make a 'super type' or *typeclass* called *`Num`* is which is a property which both *`Int`* and *`Real`* have, then we can define $x |-> 2*x$ more generally as of type *`Num a => a -> a`* which reads, for a type *`a`* with property(belonging to) *`Num`*, the function $x |-> 2*x$ has type *`a -> a`*
- Similarly, one could define a generalized version of the other functions we described.

A study of types and what we can infer from them(and how we can infer them) is called, rightfully so, *Type Theory*. It is deeply related to computational proof checking and formal verification. While we will not study about it in too much detail in this course, it is its own subject and is covered in detail in other courses.

While we recommend, atleast for the early chapters, to declare the types of your functions explicitly ex. `(+) :: Int -> Int  -> Int`; Haskell has a type inference system#footnote[Damas–Hindley–Milner Type Inference is the one used in Haskell at time of writing.] which is quite accurate and tries to go for the most general type. This can be both a blessing and curse, as we will see in a few moments.

This chapter will deal (in varying amounts of details) with the types *`Bool`*, *`Int`*, *`Integer`*, *`Float`*, *`Char`* and *`String`*.
#definition[
  *`Bool`* is a type which has only two valid values, *`True`* and *`False`*. It most commonly used as output for indicator functions(indicate if something is true or not).
]
#definition[
  *`Int`* and *`Integer`* are the types used to represent integers. 

  `Integer` can hold any number no matter how big, up to the limit of your machine's memory, while `Int` corresponds to the set of positive and negative integers that can be expressed in 32 or 64 bits(based on system) with the bounds changing depending on implementation (guaranteed at least -2^29 to 2^29). Going outside this range may give weird results. Ex. `product [1..52] :: Int` gives a negative number which cannot realistically be $52!$. On the other hand, `product [1..52] :: Integer` gives indeed the correct answer.

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

#definition[
  *`Rational`*, *`Float`* and *`Double`* are the types used to deal with non-integral numbers. The former is used for fractions or rationals while the latter for reals with varying amount of precision. Rationals are declared using `%` as the viniculum(the dash between numerator and denominator). For example `1%3, 2%5, 97%31`.

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
We can see that the prescission of $sqrt(99999)$ is much lower than that of $sqrt(2)$. We will use `Float` for most of this book.
]


#definition[
  *`Char`* are the types used to represent arbitrary Unicode characters. This includes all numbers, letters, white spaces(space, tab, newline etc) and other special characters.

  *`String`* is the type used to represent a bunch of characters chained together. Every word, sentence, paragraph is either a string or a collection of them.

  In haskell, Strings and Chars are differentiated using the type of quotation used. `"hello" :: String` as well as `"H" :: String` but `'H':: Char`. Unlike some other languages, like say Python, we can't do so interchangeably. Double Quotes for Strings and Single Quotes for Chars.
]
Similer to many modern languages, In Haskell, String is just a synonym for a list of characters that is `String` is same as `[Char]`. This allows string manipulation to be extremely easy in Haskell and is one of the reason why Pandoc, a universal document converter and one of the most used software in the world, is written in Haskell. We will try to make a mini version of this at the end of the chapter.

#definition[
  To recall, a tuple is a length immutable, ordered multi-typed data structure. This means we can store a fixed number of multiple types of data in an order using tuples. Ex.
  `(False , True ) :: (Bool, Bool)`
  `(False , 'a', True ) :: (Bool, Char, Bool)` 
  `("Yes", 5.21 , 'a') :: (String, Float, Char)`
  
  A list is a length mutable, ordered, single typed data structure. This means we can store an arbitrary number things of the same type in a certain order using lists. Ex.
  `[False, True, False] :: [Bool]`
  `['a','b','c','d'] :: [Char]`
  `["One","Two","Three"] :: [String]`
]

== Logical Operations
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
Finally, we can define use the `case .. of ..` syntax. While this syntax is rarer, and too verbose, for simple functions, we will see a lot of it later in [monads chapter]. In this syntax, the general form is
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
```
Now let's move to the slightly interesting ones. `recip` is a function with a rather interesting type signature. It is only defined on types with the `Fractional` typeclass. This refers to a lot of things, but the most common ones are `Rational, Float` and `Double`.  `recip`, as the name suggests, returns the reciprocal of the number taken as input. The type signature is `recip :: Fractional a => a -> a`
```
ghci> recip 5
0.2
ghci> k = 5 :: Int
ghci> recip k
<interactive>:47:1: error: [GHC-39999]...
```
It is clear that in the above case, 5 was treated as a `Float` or `Double` and the expected output provided. In the following case, we specified the type to be `Int` and it caused a horrible error. This is because for something to be a fractional type, we literally need to define how to reciprocate it. 

The type signature of `(\)` is as follows `Fractional a => a -> a -> a`. Here the `Fractional` typeclass is a property which defines real division over 




// cite
// Curry Howerd by Example - CJ Quines
// Programming in Haskell - Grahm Hutton
