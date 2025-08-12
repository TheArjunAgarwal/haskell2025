#metadata[
```haskell
module Book.Code.BasicSyntax where
```
]

#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise

#import "../Modules/Definition.typ" : def
#import "../Modules/Proof.typ" : proof
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Tree.typ" : tree, dots
#let d = sym.colon.eq
#let e = $==$
#let gray(x) = text(fill: luma(150), x) 
#let grad(content) = {
  set text(fill: gradient.linear(..color.map.flare))
  box(content)
}
#let love(content) = {
  set text(fill: gradient.linear(rgb(214,2,112), rgb(0, 56, 168), angle: -30deg))
  // Yes, these are bi-flag colors, tried a bunch, this looked the prettiest
  box(content)
}

We will now gradually move to actually writing in Haskell. Programmers refer to this step as learning the "syntax" of a language. 

To do this we will slowly translate the syntax of mathematics into the corresponding syntax of Haskell.

= The Building Blocks

Just like in math, the Haskell language relies on the symbols and expressions. The symbols include whatever characters can be typed by a keyboard, like `q`,`w`,`e`,`r`,`t`,`y`,`%`,`(`,`)`,`=`,`1`,`2`,etc.

= Values

Haskell has values just like in math.

#def(sub : "value")[
  A *value* is a single and specific well-defined object that is constant, i.e., does not change from scenario to scenario nor represents an arbitrary object.
]

// //suggest that we do not define type

// Haskell being strict implies that it needs to know the type of everything it deals with. For example,
// - The type of $e$ is *`Real`*.
// - The type of $2$ is *`Int`*, for integer.
// - The type of $2$ can also be *`Real`*. But the `2 :: Int` and `2 :: Real` are different, because they have different types.
// - The type of $x |-> floor(x)$ is *`Real -> Int`*, because it takes a real number to an integer.
// - We write $(x |-> floor(x)) e = 2$ By applying a function of type *`Real -> Int`* to something of type *`Real`* we get something of type *`Int`*
// - The type of $x |-> x + 2$, when it takes integers, is *`Int -> Int`*.
// - We cannot write $(x |-> x + 2)(e)$, because the types don’t match. The function wants an input of type *`Int`* but $e$ is of type *`Real`*. We could define a new function $x |-> x+2$ of type *`Real -> Real`*, but it is a different function.
// - Functions can return functions. Think of $(+)$ as a function that takes an *`Int`*, like $3$, and returns a function like $x |-> x + 3$, which has type *`Int -> Int`* Concretely, $(+)$ is $x |-> (y |-> y + x)$. This has type *`Int -> (Int -> Int)`*.
// - We write $(+)(3)(4) = 7$. First, $(+)$ has type *`Int -> (Int -> Int)`*, so $(+)(3)$ has type *`Int -> Int`*. So, $(+)(3)(4)$ should have type *`Int`*.
// - The type of $x |-> 2*x$ is *`Int -> Int`* when it takes integers to integers. It can also be *`Real -> Real`* when it takes reals to reals. These are two different functions, because they have different types. But if we make a 'super type' or *typeclass* /*suggest that we not mention typeclass yet*/ called *`Num`* is which is a property which both *`Int`* and *`Real`* have, then we can define $x |-> 2*x$ more generally as of type *`Num a => a -> a`* which reads, for a type *`a`* with property(belonging to) *`Num`*, the function $x |-> 2*xz$ has type *`a -> a`*
// - Similarly, one could define a generalized version of the other functions we described.

Examples include - 
- The number `pi` with the decimal expansion `3.141592653589793...`
// - The set of rational numbers $QQ$
- The order `<` on the `Integer`s
- The function of squaring an `Integer`
- the character `'a'` from the keyboard
- `True` and `False`

= Variables

Haskell also has its own variables.

#def(sub : "variable")[
A *variable* is a symbol or chain of symbols, \ meant to represent an arbitrary object of some #link(<intro-to-types>,[_type_]), \ 
usually as a way to show that whatever process follows is general enough so that the process can be carried out with _any arbitrary value_ from that _type_.

The following examples should clarify further.
]

We have previously seen how variables are used in function definitions and theorems.

Even though we can prove theorems about Haskell, the Haskell language itself supports only function definitions and not theorems.

So we can use variables in function definitions. For example - 
```haskell
-- | double
double :: Integer -> Integer
double x = x + x
```
This reads - "`double` is a function that takes an `Integer` as input and gives an `Integer` as output. The `double` of an input `x` is the output `x + x`"

Note that `x` here is a variable.

Also, in mathematics we would write `double`(`x`), but Haskell does not need those brackets. \ So we can simply put some space between `double` and `x`, i.e., \ we write `double x`, \ in order to indicate that `double` is the name of the function and `x` is its input. 

Also, *Note* that the names of Haskell @definition_of_variable#[s] have to begin with an lowercase English letter.

= Types <intro-to-types>

Every @definition_of_value and @definition_of_variable in Haskell must have a "type".

For example,
- `'a'` has the type `Char`, \ indicating that it is a character from the keyboard.

- `5` can have the type `Integer`, \ indicating that it is an integer.

- `double` has the type `Integer -> Integer`, \
  indicating that is a function that takes an integer as input and gives an integer as output.

- In the definition of `double` , specifically "`double x = x + x`", \
  the variable `x` has type `Integer`, \ indicating that it is an integer.

The type of an object is like a short description of the object's "nature".

Also, *Note* that the names of types usually have to begin with an uppercase English letter.

== Using GHCi to get Types

GHCi allows us to get the type of any value using the command `:type +d ` followed by the value - 

```
-- | :type +d
>>> :type +d 'a'
'a' :: Char

>>> :type +d 5
5 :: Integer

>>> :type +d double
double :: Integer -> Integer
```

`x :: T` is just Haskell's way of saying "`x` is of type `T`".

*Note - * The `+d` at the end of `:type +d` stands for "default", which means that its a more basic version of the more powerful command `:type`

For example - 
```
>>> :type +d (+)
(+) :: Integer -> Integer -> Integer
```
This reads - "The function `+` takes in two `Integer`s as inputs and gives an `Integer` as output"

Or more generally - 
```
-- | :type
>>> :type (+)   
(+) :: Num a => a -> a -> a
```
This reads - "The function `+` takes in two `Num`bers as inputs and gives a `Num`ber as output"

In summary, `:type +d` is specific, whereas `:type` is general. \ For now, we will be assuming `:type +d` throughout, until we get to Chapter 6. //todo

== Types of Functions

As we have seen before, `double` has type `Integer -> Integer`. This function has a single input.

And the "basic" type of the @definition_of_infix_binary_operator `+` is `Integer -> Integer -> Integer`. This function has two inputs.

We can also define functions which takes a greater number of inputs - 

```haskell
-- | functions with many inputs
sumOf2 :: Integer -> Integer -> Integer
sumOf2 x y = x + y
-- The above function has 2 inputs

sumOf3 :: Integer -> Integer -> Integer -> Integer
sumOf3 x y z = x + y + z
-- The above function has 3 inputs

sumOf4 :: Integer -> Integer -> Integer -> Integer -> Integer
sumOf4 x y z w = x + y + z + w
-- The above function has 4 inputs
```

So we can deduce that in general, \ if a function takes $n$ inputs of types `T1`,`T2`,`T3`,...,`Tn` respectively, \ and gives an output of type `T`, \ then the function itself will have type `T1 -> T2 -> T3 -> . . . -> Tn -> T` .

= Well-Formed Expressions

Of course, since we have @definition_of_value#[s] and @definition_of_variable#[s], we can define "well-formed expressions" in a very similar manner to what we had before - 

#def(sub:"checking whether expression is well-formed")[
  It is difficult to give a direct definition of a *well-formed expression*. 
  
  So before giving the direct definition,\  we define a _formal procedure_ to check whether an expression is a *well-formed expression* or not.

  The procedure is as follows - 

  Given an expression _$e$_, 
  
    - first check whether _$e$_ is 
      - a @definition_of_value, or
      #v(0pt,weak:true)
      - a @definition_of_variable
      #v(5pt,weak:true) in which cases _$e$_ passes the check and is a *well-formed expression*.
   
    Failing that,
 
    - check whether _$e$_ is of the form _$f(e_1,e_2,e_3,...,e_n)$_, where 
      - _$f$_ is a function
      - which takes _$n$_ inputs, and
      #v(5pt,weak:true)
      - _$e_1,e_2,e_3,...,e_n$_ are all _well-formed expressions_ which are _valid inputs_ to _$f$_.


    And only if _$e$_ passes this check will it be a *well-formed expression*.]

#def(sub:"well-formed expression")[
  An _expression_ is said to be a  *well-formed expression* if and only if it passes the formal checking procedure defined in @definition_of_checking_whether_expression_is_well-formed.
]

Recall, that last time in @math_expression, when we were formally checking that $x^3 dot x^5 + x^2 + 1$ is indeed a @definition_of_well-formed_expression, we skipped the part about checking whether #align(center)["_$e_1,e_2,e_3,...,e_n$_ are ... _valid inputs_ to _$f$_."] which is present in the very last part of the formal procedure \ @definition_of_checking_whether_mathematical_expression_is_well-formed. 

That is, we didn't have a very good way to check whether #align(center)[the input to a function $in$ the domain of the function], Thus we could potentially face mess-ups like $ (1,2) + 3 $
Here, the expression is not well-formed because $(1,2)$ is not a valid input for $+$ \ ( in other words $(1,2) in.not "the domain of" + $ )\ , but we had no way to prevent this before.

Now, with types, this problem is solved!

If a function has type `T1 -> T2`, \ and Haskell wants to check whether whatever input has been given to it is a valid input or not, \ it need only check that this input is of type `T1`.

We can see this in action with `double` - 

```
>>> double 12
24
```

`12` has type `Integer`, and therefore Haskell is quite happy to take it as input to the function `double` of type `Integer -> Integer`.

However - 

```
>>> double 'a'

<interactive>:1:8: error: [GHC-83865]
    * Couldn't match expected type `Integer' with actual type `Char'
    * In the first argument of `double', namely 'a'
      In the expression: double 'a'
      In an equation for `it': it = double 'a'
```
Since `double` has type `Integer -> Integer`, Haskell tries to check whether the input `'a'` has type `Integer`, but discovers that it actually has a different type (`Char`), and therefore disallows it.

This is actually the point of types, and the consequences are very powerful. \ 
Why? Recall that @definition_of_well-formed_expression#[s] are supposed to be only those expressions which are meaningful. Since Haskell has the power to check whether expressions are well-formed or not, it will never allow us to write a "meaningless" expression. 

Other programming languages which don't have types allows one to write these "meaningless" expressions and that creates "bugs" a.k.a logical errors.

The very powerful consequence is that Haskell manages to *provably avoid any of these logical errors*!

= Infix Binary Operators

If we enclose *an @definition_of_infix_binary_operator* in *brackets*, we can use it just as we would a *function*

```
-- | using infix operator as function
>>> 12 + 34 -- usage as infix binary operator
46
>>> (+) 12 34 -- usage as a normal Haskell function
46

>>> 12 - 34 -- as infix binary operator
-22
>>> (-) 12 34 -- usage as a normal Haskell function
-22

>>> 12 * 34 -- as infix binary operator
408
>>> (*) 12 34 -- usage as a normal Haskell function
408
```

Conversely, if we enclose a *function* in *backticks (#raw("`"))*, we can use it just like an *@definition_of_infix_binary_operator*.
```
-- | using function as infix operator
>>> f x y = x*y + x + y -- function definition
>>> f 3 4 -- usage as a normal Haskell function
19
>>> 3 `f` 4 -- usage as an infix binary operator
19
```

== Precedence

@definition_of_infix_binary_operator#[s] sometimes introduce a small complication. 
\ For example, when we write `a + b * c`, \ do we mean `a + ( b * c )` \ or do we mean `( a + b ) * c` ?

We know that the method to solve these problems are the BODMAS or PEMDAS conventions.

So Haskell assumes the first option due to BODMAS or PEMDAS conventions, whichever one takes your fancy.

This problem is called the problem of "precedence", i.e., \ "which operations in an expression are meant to be applied first (preceding) and which to be applied later?"

Haskell has a convention for handling all possible @definition_of_infix_binary_operator#[s] that extends the PEMDAS convention. \
(It assigns to each @definition_of_infix_binary_operator a number indicating the precedence, and those with greater value of precedence are evaluated first)

But there still remains an issue - 

What about `a - b - c`? \ Does it mean `( a - b ) - c` ,\ or does it mean `a - ( b - c )`?

Observe that this issue is not solved by the BODMAS or PEMDAS convention.

Haskell chooses `( a - b ) - c`, because `-` is "left-associative". 

#def(sub:"left-associative")[
  If an @definition_of_infix_binary_operator `⨝` is *left-associative*, it means that the expression 
```
x₁ ⨝ x₂ ⨝ x₃ ⨝ ... ⨝ xₙ
```
  is equivalent to 
```
( x₁ ⨝ x₂ ) ⨝ x₃ ⨝ ... ⨝ xₙ
```
  which means that the leftmost `⨝` is evaluated first.
]

Therefore `a - b - c` is equivalent to `( a - b ) - c` and not `a - ( b - c )`.

But what about `a - b - c - d`?

```
a - b - c - d
-- take ⨝ as -, n as 4, x₁ as a, x₂ as b, x₃ as c, x₄ as d
== ( a - b ) - c - d 
-- take ⨝ as -, n as 3, x₁ as ( a - b ), x₂ as c, x₃ as d
== ( ( a - b ) - c ) - d
```

#exercise(sub:"order of operations")[
  Find out the value of `7 - 8 - 4 - 15 - 65 - 42 - 34`
]

We also have the complementary notion of being "right-associative".

#def(sub:"right-associative")[
  If an @definition_of_infix_binary_operator `⨝` is *right-associative*, it means that the expression 
```
x₁ ⨝ x₂ ⨝ x₃ ⨝ ... ⨝ xₙ₋₂ ⨝ xₙ₋₁ ⨝ xₙ
```
  is equivalent to 
```
x₁ ⨝ x₂ ⨝ x₃ ⨝ ... ⨝ xₙ₋₂ ⨝ ( xₙ₋₁ ⨝ xₙ )
```
  which means that the rightmost `⨝` is evaluated first.
]

// #def(sub:"associative")[
//   If it doesn't matter whether an operator is @definition_of_left-associative or @definition_of_right-associative, then that operator is called *associative*.
// ]

// For example, it doesn't matter whether `+` is @definition_of_left-associative or @definition_of_right-associative, because either convention leads to the same result-
// ```
// ( a + b ) + c == a + ( b + c )
// ```
// Therefore `+` is @definition_of_associative.

// If an operator is @definition_of_associative, then it doesn't matter in which order its appearances are evaluated, all choices of the order of evaluation will lead to the same result.

= Logic

== Truth

The way to represent truth or falsity in Haskell is to use the value `True` or the value `False` respectively. Both values are of type `Bool`.

```
>>> :type True
True :: Bool

>>> :type False
False :: Bool
```
The `Bool` type means "true or false".

The values `True` and `False` are called `Bool`eans.

== Statements

Haskell can check the correctness of some very simple mathematical statements - 

```
-- | simplest logical statements
>>> 1 < 2
True

>>> 2 < 1
False

>>> 5 == 5
True

>>> 5 /= 5
False

>>> 4 == 5
False

>>> 4 /= 5
True
```
#import "../Modules/Book.typ" : unligate
*Note* that `/=` is written as #unligate(`/=`)\
*Note* that `<=` is written as #unligate(`<=`)\
etc.

But the very nice fact is that Haskell does not require any new syntax or mechanism for these.

The way Haskell achieves this is an inbuilt @definition_of_infix_binary_operator named `<`, \ which takes two inputs,`x` and `y`, \ and outputs `True` if `x` is less than `y`, and otherwise outputs `False`.

So, in the statement `1 < 2`, \ the `<` function is given the two inputs `1` and `2`, \ and then GHCi evaluates this and outputs the correct value, `True`.

```
>>> 1 < 2
True
```

So let's see if all this makes sense with respect to the type of `<`  -
```
-- | type of <
>>> :type (<)   
(<) :: Ord a => a -> a -> Bool
```
Indeed we see that `<` takes two inputs of type `a`, and gives an output of type `Bool`.

= Conditions

So we can use these functions to define some "condition" on a @definition_of_variable. 

For example - 
```haskell
-- | condition on a variable
isLessThan5 :: Integer -> Bool
isLessThan5 x = x < 5
```
This function encodes the "condition" that the input variable must be less than $5$.

However, we would definitely like to express some more complicated conditions as well. For example, we might want to express the condition - $ x in (4,10] $

We know that $x in (4,10]$ if and only both $x > 4 "AND" x <= 10$ hold true.

Using this fact, we can express the condition "$x in (4,10]$" as $ #[` ( x > 4 ) && ( x <= 10 ) `] $ in Haskell, since `&&` represents "AND" in Haskell.

Let's take`x` to be `7` and see what is happening here step by step - 
```
   ( x > 4 ) && ( x <= 10 )
== ( 7 > 4 ) && ( 7 <= 10 )
==    True   && ( 7 <= 10 )
==    True   &&    True
-- now applying the definition of && aka AND
== True
```
which is correct since " $7 in (4,10]" "$" is indeed a true statement.

So the type of `&&` is - 
```
>>> :type (&&)
(&&) :: Bool -> Bool -> Bool
```
It takes two `Bool`eans as inputs and outputs another `Bool`ean.

== Logical Operators

#def(sub: "logical operator")[
_Functions_ like `&&`, which take in some `Bool`ean(s) as input(s), and give a single `Bool`ean as output are called *logical operators*. 
]

You might have seen some logical operators before with names such AND, OR, NOT, NAND, NOR etc.

As we just saw, they are very useful in combining two conditions into one, more complicated condition.

For example - 
- if we want to express the condition $ x in (-oo,6] union (15,oo) $, we would re-express it as #align(center)["$x <= 6$ OR $x > 15$"], which could finally be expressed in Haskell as #align(center)[`( x <= 6 ) || ( x > 15 )`], since `||` is Haskell's way of writing OR.
\ \ 
- if we want to express the condition $ x in.not (-oo,4) $, we could re-express it as $ "NOT" ( x in (-oo,4) ) $, which could be further re-written as $ "NOT" ( x < 4 ) $, which then can be expressed in Haskell as #align(center)[`not ( x < 4 )`]

We include the definition of `not` as it is quite simple -
```
-- | not
not :: Bool -> Bool
not True  = False
not False = True
```

=== Exclusive OR aka XOR

Finally, we define a logical operator called XOR. 
#def(sub: "XOR")[
  *( boolean$""_1$ XOR boolean$""_2$ )* is defined to be true\ if and only if\ _at least one of the 2 inputs is true, but not both_,\
  and otherwise is defined to be false.
]

Suppose P and Q are two people running a race against each other.

Then at least one of them will win, but not both.

Therefore ( ( A wins ) XOR ( B wins ) ) would be true.

Also, ( false XOR false ) would be false, since at least one of the inputs need to be true.

Finally, ( true XOR true ) would be false, as both inputs are true.

= Function Definitions

Functions are a very important tool in mathematics and they form the foundations of Haskell programming. \ 

Nearly everything in Haskell is done using functions, so there various ways of defining many kinds of functions.

== Using Expressions

In its simplest form, a function definition is made up of a `left-hand side` describing the function name and input(s), `=` in the middle and a `right-hand side` describing the output.

An example -

If we want write the following definition $ f(x,y) := x^3 dot x^5 + y^3 dot x^2 + 14 $
Then we can write in Haskell -
```
-- | basic function definition
f x y = x^3 * x^5 + y^3 * x^2 + 14
```

On the left we write the name of the function followed by a number of variables which represent its inputs.

In the middle we write `=`, indicating that right-hand side is the definition of the left-hand side.

On the right, we write a @definition_of_well-formed_expression using the variables of the left-hand side, describing to how to combine and manipulate the inputs to form the output of the function.

Also, we know that $ f : ZZ times ZZ -> ZZ$

We can include this information in the definition -
```haskell
-- | function definition with explicit type
f :: Integer -> Integer -> Integer
f x y = x^3 * x^5 + y^3 * x^2 + 14
```

Even though it is not mandatory, it is *always* advised to follow the above style and *explicitly provide a particular type* for the function being defined. 

Even if an explicit type is not provided, Haskell will assume the most general type the function could have, like what we observed in the `:type` command of GHCi.

Let's try to define @definition_of_XOR in Haskell -

```haskell
-- | xor
xor :: Bool -> Bool -> Bool
xor b1 b2 =
--     at least one of the inputs is True, but   not both
-- <=> b1 is True OR b2 is True          , but   not both
-- <=> ( b1 == True ) OR ( b2 == True )  , but   not both
-- <=> ( b1 == True ) OR ( b2 == True )  , but   not ( b1 AND b2 )
-- <=> ( b1 == True ) OR ( b2 == True )  , but ( not ( b1 AND b2 ) )
-- <=> ( b1 == True ) OR ( b2 == True )   AND  ( not ( b1 AND b2 ) )
     ( ( b1 == True ) || ( b2 == True ) ) &&   ( not ( b1 &&  b2 ) )
```

== Some Conveniences

=== Piecewise Functions 

If we have a function definition like
$
  "< functionName > "(x) #d cases(
    "< expression"_1" >" &"; if < condition"_1" >","",
    "< expression"_2" >" &"; if < condition"_2" >","",
    "< expression"_3" >" &"; if < condition"_3" >","",
    .,.,.,"",
    "< expression"_N" >" &"; if < condition"_N" >",
  )
$
, it can be written in Haskell as
```
-- | guards
functionName
    | condition1 = expression1
    | condition2 = expression2
    | condition3 = expression3
    .
    .
    .
    | conditionN = expressionN
```

For example,
$
  "signum" : RR &-> RR\
  "signum"(x) &#d cases( 
    +1 &"; if "x " "> &&0 , "",
    "  "0 &"; if "x == &&0 , "" ,
    -1 &"; if "x " "< &&0
    )
$
can written in Haskell as
```
-- | basic usage of guards
signum :: Double -> Double
signum x
    | x >  0 =  1
    | x == 0 =  0
    | x <  0 = -1
```

If a piecewise definition has a "catch-all" or "otherwise" clause at the end, as in
$
  "< functionName > "(x) #d cases(
    "< expression"_1" >" &"; if < condition"_1" >","",
    "< expression"_2" >" &"; if < condition"_2" >","",
    "< expression"_3" >" &"; if < condition"_3" >","",
    .,.,.,"",
    "< expression"_N" >" &"; if < condition"_N" >","",
    "< expression"_(N+1)" >" &"; otherwise"
  )
$
, it can be written in Haskell as
```
-- | guards
functionName
    | condition1 = expression1
    | condition2 = expression2
    | condition3 = expression3
    .
    .
    .
    | conditionN = expressionN
    | otherwise  = expression(N+1)
```
This `|` syntax symbol is called a "guard".

For example - 

```haskell
-- | otherwise
xor1 :: Bool -> Bool -> Bool
xor1 b1 b2
    | (not b1) && (not b2) = False -- when none of the inputs are True
    | b1 && b2             = False -- when both of the inputs are True
    | otherwise            = True  -- any other situation
```


If a piecewise definition has ony two parts
$
  "< functionName > "(x) #d cases(
    "< expression"_1" >" &"; if < condition >",
    "< expression"_2" >" &"; otherwise"
  )
$
then a lot programming languages have a simple construct called "if-else" to express this -
```
-- | if-then-else
functionName = if condition then expression1 else expression2
```
For example -
```haskell
-- | if-then-else example
xor2 :: Bool -> Bool -> Bool
xor2 b1 b2 = if b1 == b2 then False else True
-- if both inputs to xor are the same, then output False, otherwise True
```

=== Pattern Matching

We can write the map of every possible input one by one. This is called "exhaustive pattern matching".

```haskell
-- | exhaustive pattern matching
xor3 :: Bool -> Bool -> Bool -- answer True iff at least one input is True, but not both
xor3 False False = False -- at least one input should be True
xor3 True  True  = False -- since both inputs are True
xor3 False True  = True
xor3 True  False = True
```

We could be smarter and save some keystrokes.

```haskell
-- | pattern matching
xor4 :: Bool -> Bool -> Bool
xor4 False b = b
xor4 b False = b
xor4 b1 b2 = False
```

Another small pattern match equivalent to `xor1` -

```haskell
-- | unused variables in pattern match
xor5 :: Bool -> Bool -> Bool
xor5 False False = False
xor5 True True = False
xor5 b1 b2 = True
```

But since the variables `b1` and `b2` are not used in the right-hand side, \ we can replace them with `_` (read as "wildcard")

```haskell
-- | wildcard
xor6 :: Bool -> Bool -> Bool
xor6 False True = True
xor6 True False = True
xor6 _ _ = False
```

Wildcard (`_`) just means that any pattern will be accepted.

We can use other functions to help us as well -
```haskell
-- | using other functions in RHS
xor7 :: Bool -> Bool -> Bool
xor7 False b = b
xor7 True b = not b
```
We can also piecewise definitions in a pattern match -
```haskell
-- | pattern matches mixed with guards
xor8 :: Bool -> Bool -> Bool
xor8 False b2 = b2 -- Notice, we can have part of the definition unguarded before entering the guards.
xor8 True b2
  | b2 == False = True
  | b2 == True  = False
```

Now we introduce the `case..of..` syntax. It is used to pattern-matching for any expression, not necessarily just the input variables, which are the only kinds of examples we've seen till now.
```
case <expression> of
  <pattern1> -> <result1>
  <pattern2> -> <result2>
  ...
```
The case syntax evaluates the `<expression>`, and matches it against each pattern in order. The first matching pattern's corresponding result is returned.

```haskell
-- | trivial case
xor9 :: Bool -> Bool -> Bool
xor9 b1 b2 = case b1 of
  False -> b2
  True  -> not b2
```

```haskell
-- | non-trivial case
xor10 :: Bool -> Bool -> Bool
xor10 b1 b2 = case ( b1 , b2 ) of
    ( False , False ) -> False
    ( True  , True  ) -> False
    _                 -> True
```

=== Where, Let

```haskell
-- | where
xor11 :: Bool -> Bool -> Bool
xor11 b1 b2 = atLeastOne && (not both)
    where
        atLeastOne = b1 || b2
        both = b1 && b2
```

```haskell
-- | let
xor12 :: Bool -> Bool -> Bool
xor12 b1 b2 = 
    let
        atLeastOne = b1 || b2
        both = b1 && b2
    in
        atLeastOne && (not both)
```

=== Without Inputs

Let us recall for a moment the definition for `xor2` (in @code_of_if-then-else_example)

#text(size:0.7em)[#context(query([@code_of_if-then-else_example].target).at(0))]

We can see that this just equivalent to 

```haskell
xor13 :: Bool -> Bool -> Bool
xor13 b1 b2 = not ( b1 == b2 )
```

which can be shortened even further

```haskell
xor14 :: Bool -> Bool -> Bool
xor14 b1 b2 = b1 /= b2
```
, rewritten by @code_of_using_infix_operator_as_function

```haskell
xor15 :: Bool -> Bool -> Bool
xor15 b1 b2 = (/=) b1 b2
```
and thus can finally be shortened to the extreme
```haskell
-- | function definition without input variables
xor16 :: Bool -> Bool -> Bool
xor16 = (/=) 
```
Notice the curious thing that the above function definition doesn't have any input variables. This ties into a fundamentally important concept called currying which we will explore later.

=== Anonymous Functions

An anonymous function like

$
  ( x |-> x^3 dot x^5 + x^2 + 1 ) : RR -> RR
$

can written as 
```
-- | basic anonymous function
( \ x -> x^3 * x^5 + x^2 + 1 ) :: Double -> Double
```

Note that we used $->$ in place of $|->$,\ and also added a `\` (pronounced "lambda") before the input variable.

// This style is particularly useful when we (for some reason) do not want name the function.

For an example with multiple inputs, consider
$
  ( x , y |-> 1/x + 1/y )
$
which can be written as 
```
-- | multi-input anonymous function
( \ x y -> 1/x + 1/y )
```

#exercise(sub : "only nand")[
  It is a well know fact that one can define all logical operators using only `nand`. Well, let's do so. 
  
  Redefine `and`, `or`, `not` and `xor` using only `nand`.
] 

== Recursion

A lot of mathematical functions are defined recursively. We have already seen a lot of them in chapter 1 and exercises. Factorial, binomials and fibonacci are common examples.

We can use the recurrence 
$
  n! := n dot (n-1)!
$
to define the factorial function.
```haskell
-- | factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)
```

We can use the standard Pascal's recurrence
$
  vec(n,r) := vec(n-1,r) + vec(n-1,r-1)
$
to define the binomial or "choose" function.

```haskell
-- | binomial
choose :: Integer -> Integer -> Integer
0 `choose` 0 = 1
0 `choose` _ = 0
n `choose` r = (n-1) `choose` r + (n-1) `choose` (r-1)
```
And we have already seen the recurrence relation for the fibonacci function in @math-recursion.
```haskell
-- | naive fibonacci definition
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

= Optimization

For fibonacci, note that in @code_of_naive_fibonacci_definition is, well, naive.

This is because we keep recomputing the same values again and again. For example computing `fib 5` according to this scheme would look like:
```
-- | computation of naive fibonacci
fib 5 == fib 4 + fib 3
      == (fib 3 + fib 2) + (fib 2 + fib 1)
      == ((fib 2 + fib 1) + (fib 1 + fib 0)) + ((fib 1 + fib 0) + 1)
      == (((fib 1 + fib 0) + 1) + (1 + 1)) + ((1 + 1) + 1)
      == (((1 + 1) + 1) + (1 + 1)) + ((1 + 1) + 1)
      == 8
```

If we can manage to avoid recomputing the same values over and over again, then the computation will take less time.

That is what the following definition achieves.

```haskell
-- | fibonacci by tail recursion
fibonacci :: Integer -> Integer
fibonacci n = go n 1 1 where
  go 0 a _ = a
  go n a b = go (n - 1) b (a + b)
```
We can see that this is much more efficient. Tracing the computation of `fibonacci 5` now looks like:
```
-- | computation of tail recursion fibonacci
fibonacci 5 
== go 5 1 1
== go 4 1 2
== go 3 2 3
== go 2 3 5
== go 1 5 8
== go 0 8 13
== 8
```
This is called tail recursion as we carry the tail of the recursion to speed things up. It can be used to speed up naive recursion, although not always.

Another way to evaluate fibonacci will be seen in end of chapter exercises, where we will translate Binet's formula straight into Haskell. Why can't we do so directly? As we can't represent $sqrt(5)$ exactly and the small errors in the approximation will accumulate due to the number of operations. This exercise should allow you to end up with a blazingly fast algorithm which can compute the $12.5$-th million fibonacci number in 1 sec. Our tail recursive formula takes more than 2 mins to reach there.

= Numerical Functions

#def(sub:"Integer and Int")[
  *`Int`* and *`Integer`* are the types used to represent integers. 

  `Integer` can hold any number no matter how big, up to the limit of your machine's memory, while `Int` corresponds to the set of positive and negative integers that can be expressed in 32 or 64 bits(based on system) with the bounds changing depending on implementation (guaranteed at least $-2^29$ to $2^29$). Going outside this range may give weird results. 

  The reason for `Int` existing is historical. It was the only option at one point and continues to be available for backwards compatibility.

  We will assume `Integer` wherever possible.
]

#def(sub:"Rational")[
  *`Rational`* and *`Double`* are the types used to deal with non-integral numbers. The former is used for fractions or rationals while the latter for reals with varying amount of precision. 
  
  `Rational`s are declared using `%` as the vinculum(the dash between numerator and denominator). For example `1%3, 2%5, 97%31`, which respectively correspond to $1/3,2/5,97/31$ .
]  

#def(sub:"Double")[
  `Double` or Double Precision Floating Point are high-precision approximations of real numbers. For example, consider the "square root" function - 
```
>>> sqrt 2 :: Double
1.4142135623730951

>>> sqrt 99999 :: Double
316.226184874055

>>> sqrt 999999999 :: Double
31622.776585872405
```
]

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

`recip` is a function which reciprocates a given number, but it has rather interesting type signature. It is only defined on types with the `Fractional` "type-class". This refers to a lot of things, but the most common ones are `Rational, Float` and `Double`.  `recip`, as the name suggests, returns the reciprocal of the number taken as input. The type signature is `recip :: Fractional a => a -> a`
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
Again, notice the type signature of `(/)` is `Fractional a => a -> a -> a`. #footnote("It is worth pointing out that one could define `recip` using `(/)` as well given 1 is defined. While this is not standard, if `(/)` is defined for a data type, Haskell does automatically infer the reciprocation. So technically, for a datatype to be a member of the type class `Fractional` it needs to have either reciprocation or division defined, the other is inferred.")

However, suppose that we want to do integer division and we want a quotient and remainder. 

Say we want only the quotient, then we have `div` and `quot` functions. 

These functions are often coupled with `mod` and `rem` are the respective remainder functions. We can get the quotient and remainder at the same time using `divMod` and `quotRem` functions. A simple example of usage is
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

While the functions work similarly when the divisor and dividend are of the same sign, they seem to diverge when the signs don't match.

The thing here is we always want our division algorithm to satisfy $d * q + r = n, |r| < |d|$ where $d$ is the divisor, $n$ the dividend, $q$ the quotient and $r$ the remainder. 

The issue is for any $- d < r < 0 => 0 < r < d$. This means we need to choose the sign for the remainder. 

In Haskell, `mod` takes the sign of the divisor (comes from floored division, same as Python's `%`), while `rem` takes the sign of the dividend (comes from truncated division, behaves the same way as Scheme's `remainder` or C's `%`.).

Basically, `div` returns the floor of the true division value (recall $floor(-3.56) = -4$) while `quot` returns the truncated value of the true division (recall $op("truncate")(-3.56) = -3$ as we are just truncating the decimal point off). The reason we keep both of them in Haskell is to be comfortable for people who come from either of these languages. 

Also, The `div` function is often the more natural one to use, whereas the `quot` function corresponds to the machine instruction on modern machines, so it's somewhat more efficient (although not much, I had to go upto $10^100000$ to even get millisecond difference in the two).

A simple exercise for us now would be implementing our very own integer division algorithm. We begin with a division algorithm for only positive integers.
```
-- | A division algorithm on positive integers by repeated subtraction
divide :: Integer -> Integer -> (Integer, Integer)
divide n d = go 0 n where
  go q r = if r >= d then go (q+1) (r-d) else (q,r)
```

Now, how do we extend it to negatives by a little bit of case handling?
```
divideComplete :: Integer -> Integer -> (Integer, Integer)
divideComplete _ 0 = error "DivisionByZero"
divideComplete n d

  | d < 0     = let (q, r) = divideComplete n (-d) in 
                (-q, r)

  | n < 0     = let (q, r) = divideComplete (-n) d in 
                if r == 0 then (-q, 0) else (-q - 1, d - r)

  | otherwise = divide n d

divide :: Integer -> Integer -> (Integer, Integer)
divide n d = go 0 n where
  go q r = if r >= d then go (q+1) (r-d) else (q,r)
```
#exercise(sub : "Another Division")[
  Figure out which kind of division have we implemented above, floored or truncated.

  Now implement the other one yourself by modifying the above code appropriately.
]

== Exponentiation
Haskell defines for us three exponentiation operators, namely `(^^), (^), (**)`. 

#exercise(sub : "Can you see the difference?")[
  What can we say about the three exponentiation operators?
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

Unlike division, they have almost the same function. The difference here is in the type signature. While, inhering the exact type signature was not expected, we can notice:
- `^` is raising general numbers to positive integral powers. This means it makes no assumptions about if the base can be reciprocated and just produces an exception if the power is negative and error if the power is fractional.
- `^^` is raising fractional numbers to general integral powers. That is, it needs to be sure that the reciprocal of the base exists (negative powers) and doesn't throw an error if the power is negative.
- `**` is raising numbers with floating point to powers with floating point. This makes it the most general exponentiation.

The operators clearly get more and more general as we go down the list but they also get slower. However, they are also reducing in accuracy and may even output `Infinity` in some cases. The `...` means I am truncating the output for readability, GHCi did give the complete answer.

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
-- | A naive integer exponentiation algorithm
exponentiation :: (Num a, Integral b) => a -> b -> a
exponentiation a 0 = 1
exponentiation a b = if b < 0 
  then error "no negative exponentiation" 
  else a * (exponentiation a (b-1))
```
This algorithm, while the most naive way to do so, computes $2^100000$ in merely $0.56$ seconds.

However, we could do a bit better here. Notice, to evaluate $a^b$, we are making $b$ multiplications. 

A fact, which we shall prove in chapter 10, is that multiplication of big numbers is faster when it is balanced, that is the numbers being multiplied have similar number of digits.

So to do better, we could simply compute $a^(b/2)$ and then square it, given $b$ is even, or compute $a^((b-1)/2)$ and then square it and multiply by $a$ otherwise. This can be done recursively till we have the solution. 
```
-- | A better exponentiation algorithm using divide and conquer
exponentiation :: (Num a, Integral b) => a -> b -> a
exponentiation a 0 = 1
exponentiation a b 
  | b < 0     = error "no negative exponentiation"
  | even b    = let half = exponentiation a (b `div` 2)
                in half * half
  | otherwise = let half = exponentiation a (b `div` 2)
                in a * half * half
```
The idea is simple: instead of doing $b$ multiplications, we do far fewer by solving a smaller problem and reusing the result. While one might not notice it for smaller $b$'s, once we get into the hundreds or thousands, this method is dramatically faster.

This algorithm brings the time to compute $2^100000$ down to $0.07$ seconds. 

The idea is that we are now making at most $3$ multiplications at each step and there are at most $ceil(log_2(b))$ steps. This brings us down from $b$ multiplications to $3 log(b)$ multiplications. Furthermore, most of these multiplications are somewhat balanced and hence optimized.

This kind of a strategy is called divide and conquer. You take a big problem, slice it in half, solve the smaller version, and then stitch the results together. It’s a method/technique that appears a lot in Computer Science (in sorting, in searching through data, in even solving differential equations and training AI models) and we will see it again shortly.

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

Let's say we want to find $g := gcd(p,q)$ and $p > q$. That would imply $p = d q + r$ for some $r < q$. This means $g | p, q => g | q, r$ and by the maximal-ity of $g$, $gcd(p,q) = gcd(q,r)$. This helps us out a lot as we could eventually reduce our problem to a case where the larger term is a multiple of the smaller one and we could return the smaller term then and there. This can be implemented as:
```
-- | Fast GCD and LCM
gcd :: Integer -> Integer -> Integer
gcd p 0 = p -- Using the fact that the moment we get q | p, we will reduce to this case and output the answer.
gcd p q = gcd q (p `mod` q)


lcm :: Integer -> Integer -> Integer
lcm p q = (p * q) `div` (gcd p q)
```

We can see that this is much faster. The exact number of steps or time taken is a slightlty involved and not very related to what we cover. Interested readers may find it and related citations #link("https://en.wikipedia.org/wiki/Euclidean_algorithm#Algorithmic_efficiency")[here].

This algorithm predates computers by approximately 2300 years. It was first described by Euclid and hence is called the Euclidean Algorithm. While, faster algorithms do exist, the ease of implementation and the fact that the optimizations are not very dramatic in speeding it up make Euclid the most commonly used algorithm.

While we will see these class of algorithms, including checking if a number is prime or finding the prime factorization, these require some more weapons of attack we are yet to develop.

== Dealing with Characters
We will now talk about characters. Haskell packs up all the functions relating to them in a module called `Data.Char`. We will explore some of the functions there.

So if you are following along, feel free to enter `import Data.Char` in your GHCi or add it to the top of your haskell file.

The most basic and important functions here are `ord` and `chr`. Characters, like the ones you are reading now, are represented inside a computer using numbers. These numbers are part of a standard called ASCII (American Standard Code for Information Interchange), or more generally, Unicode.

In Haskell, the function `ord :: Char -> Int` takes a character and returns its corresponding numeric code. The function `chr :: Int -> Char` does the inverse: it takes a number and returns the character it represents.
```
>>> ord 'g'
103
>>> ord 'G'
71
>>> chr 71
'G'
>>> chr 103
'g'
```

= Mathematical Functions
We will now talk about mathematical functions like `log`, `sqrt`, `sin`, `asin` etc. We will also take this opportunity to talk about real exponentiation. To begin, Haskell has a lot of pre-defined functions.

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
All the functions above have the type signature `Fractional a => a -> a ` or for our purposes `Float -> Float`. Also, notice the functions are not giving exact answers in some cases and instead are giving approximations. These functions are quite unnatural for a computer, so we surely know that the computer isn't processing them. So what is happening under the hood?
== Binary Search
#def(sub : "Hi-Lo game")[
  You are playing a number guessing game with a friend. Your friend is thinking of a number between $1$ and $k$, and you have to guess it. After every guess, your friend will say whether your guess is too high, too low, or correct. Prove that you can always guess the number in $ceil(log_2(k))$ guesses.
]

This follows from choosing $k/2$ and then picking the middle element of this smaller range. This would allow us to find the number in $ceil(log_2(k))$ queries.

This idea also works for slightly less direct questions:

#exercise(sub : "Hamburgers (Codeforces 371C)")[
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
burgers (3,2,1) (6,4,1) (1,2,3)                    4  = 2
burgers (2,0,1) (1,10,1) (1,10,1)                 21  = 7
burgers (1,1,1) (1,1,1) (1,1,3)        1000000000000  = 200000000001
```
]

This question may look like a combinatorics or recursion question, but any of those approaches will be very inefficient. 

Let's try to algebraically compute how much money is needed to make $x$ burgers. We can define this cost function as cost times the number of ingredient required minus the amount already in pantry. This will something like:
$
  f(x) = p_b max(0, x dot B - n_b) + p_s max(0, x dot S - n_s) + p_c max(0, x dot C - n_c)
$
And now we want to look for maximal $x$ such that $f(x) <= r$. Well, that can be done using Binary search!
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
Here ia a similar exercise for your practice.

#exercise(sub : "House of Cards (Codeforces 471C)")[
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

The reason we are interested in this methodology is as we could do this to find roots of polynomials, especially roots. How? 

While using a raw binary search for roots would be impossible as the exact answer is seldom rational and hence, the algorithm would never terminate. So instead of searching for the exact root, we look for an approximation by keeping some tolerance. Here is what it looks like:

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
  Write a function `bsCbrt :: Float -> Float -> Float` which calculates the cube root of a number upto some tolerance using binary search.
]

The internal implementation sets the tolerance to some constant, defining, for example as `sqrt = bsSqrt 0.00001 `

Furthermore, there is a faster method to compute square roots and cube roots(in general roots of polynomials), which uses a bit of analysis. You will find it defined and walked-through in the back exercise.

== Taylor Series

We know that $ln(1+x) = x - x^2/2 + x^3/3 - dots$. For small $x, ln(1+x) approx x$. So if we can create a scheme to make $x$ small enough, we could get the logarithm by simply multiplying. Well, $ln(x^2) = 2 ln(|x|)$. So, we could simply keep taking square roots of a number till it is within some error range of $1$ and then simply use the fact $ln(1+x) approx x$ for small $x$.
```haskell
-- | Log defined using Taylor Approximation
logTay :: Float -> Float -> Float
logTay tol n 
  | n <= 0                      = error "Negative log not defined"
  | abs(n - 1) <= tol           = n - 1  -- using log(1 + x) ≈ x
  | otherwise                   = 2 * logTay tol (sqrt n)
```
This is a very efficient algorithm for approximating `log`. Doing better requires the use of either pre-computed lookup tables(which would make the programme heavier) or use more sophisticated mathematical methods which while more accurate would slow the programme down. There is an exercise in the back, where you will implement a state of the art algorithm to compute `log` accurately upto 400-1000 decimal places.

Finally, now that we have `log = logTay 0.0001`, we can easily define some other functions.
```
logBase a b = log(b) / log(a)
exp n = if n == 1 then 2.71828 else (exp 1) ** n
(**) a b = exp (b * log(a))
```

We will use this same Taylor approximation scheme for `sin` and `cos`. The idea here is: $sin(x) approx x$ for small $x$ and $cos(x) = 1$ for small $x$. Furthermore, $sin(x+2pi) = sin(x)$, $cos(x + 2 pi) = cos(x)$ and $sin(2x) = 2 sin(x) cos(x)$ as well as $cos(2x) = cos^2(x) - sin^2(x)$.

This can be encoded as
```haskell
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

As one might notice, this approximation is somewhat poorer in accuracy than `log`. This is due to the fact that the taylor approximation is much less truer on `sin` and `cos` in the neighborhood of `0` than for `log`. 

We will see a better approximation once we start using lists, using the power of the full Taylor expansion. 

Finally, similar to our above things, we could simply set the tolerance and get a function that takes an input and gives an output, name it `sin` and `cos` and define `tan x = (sin x) / (cos x)`.

#exercise(sub : "Inverse Trig")[
Use taylor approximation and trigonometric identities to define inverse sin(`asin`), inverse cos(`acos`) and inverse tan(`atan`).
]

= Exercises
#exercise(sub: "Collatz")[
  Collatz conjecture states that for any $n in NN$ exists a $k$ such that  $c^k(n) = 1$ where $c$ is the Collatz function which is $n/2$ for even $n$ and $3n + 1$ for odd $n$.

  Write a function `col :: Integer -> Integer` which, given a $n$, finds the smallest $k$  such that $c^k(n) = 1$, called the Collatz chain length of $n$.
]

#exercise(sub: "Newton–Raphson method")[
#def(sub: "Newton–Raphson method")[
  Newton–Raphson method is a method to find the roots of a function via subsequent approximations.
  
  Given $f(x)$, we let $x_0$ be an initial guess. Then we get subsequent guesses using
  $
    x_(n+1) = x_n - f(x_n)/(f'(x_n))
  $
  As $n -> oo$, $f(x_n) -> 0$.

  The intuition for why this works is: imagine standing on a curve and wanting to know where it hits the x-axis. You draw the tangent line at your 
  current location and walk down it to where it intersects the x-axis. That’s your next guess. Repeat. If the curve behaves nicely, you converge quickly to the root.
  
  Limitations of Newton–Raphson method are
  - Requires derivative: The method needs the function to be differentiable and requires evaluation of the derivative at each step.
  - Initial guess matters: A poor starting point can lead to divergence or convergence to the wrong root.
  - Fails near inflection points or flat slopes: If $f'(x)$ is zero or near zero, the method can behave erratically.
  - Not guaranteed to converge: Particularly for functions with multiple roots or discontinuities.
]
Considering, $f(x) = x^2 - a$ and $f(x) = x^3 - a$ are well behaved for all $a$, implement `sqrtNR :: Float -> Float -> Float` and `cbrtNR :: Float -> Float -> Float` which finds the square root and cube root of a number upto a tolerance using the Newton–Raphson method.

Hint: The number we are trying to get the root of is a sufficiently good guess for numbers absolutely greater than $1$. Otherwise, $1$ or $-1$ is a good guess. We leave it to your mathematical intuition to figure out when to use what.
]

#exercise(sub:"Digital Root")[
  The digital root of a number is the digit obtained by summing digits until you get a single digit. For example `digitalRoot 9875 = digitalRoot (9+8+7+5) = digitalRoot 29 = digitalRoot (2+9) = digitalRoot 11 = digitalRoot (1+1) = 2`. 
  
  Implement the function `digitalRoot :: Int -> Int`.
  ]
#exercise(sub: "AGM Log")[
  A rather uncommon mathematical function is AGM or arithmetic-geometric mean. For given two numbers, 
  $
  op("AGM")(x,y) = cases(
    x & text("if") x = y\
    op("AGM") ((x+y)/2, sqrt(x y)) & text("otherwise")
  )
  $
  Write a function `agm :: (Float, Float) -> Float -> Float` which takes two floats and returns the AGM within some tolerance(as getting to the exact one recursively takes, about infinite steps).

  Using AGM, we can define
  $
    ln(x) approx pi/(2 op("AGM")(1, 2^(2-m)/x)) - m ln(2)
  $
  which is precise upto $p$ bits where $x 2^m > 2^(p/2)$.

  Using the above defined `agm` function, define `logAGM :: Int -> Float -> Float -> Float` which takes the number of bits of precision, the tolerance for `agm` and a number greater than $1$ and gives the natural logarithm of that number.

  Hint: To simplify the question, we added the fact that the input will be greater than $1$. This means a simplification is taking `m = p/2` directly. While getting a better `m` is not hard, this is just simpler.
]
#exercise(sub: "Multiplexer")[
  A multiplexer is a hardware element which chooses the input stream from a variety of streams.

  It is made up of $2^n + n$ components where the $2^n$ are the input streams and the $n$ are the selectors.

  (i) Implement a 2 stream multiplex `mux2 :: Bool -> Bool -> Bool -> Bool` where the first two booleans are the inputs of the streams and the third boolean is the selector. When the selector is `True`, take input from stream $1$, otherwise from stream $2$.

  (ii) Implement a 2 stream multiplex using only boolean operations.
  
  (iii) Implement a 4 stream multiplex. The type should be `mux4 :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool`. (There are 6 arguments to the function, 4 input streams and 2 selectors). We encourage you to do this in atleast 2 ways (a) Using boolean operations (b) Using only `mux2`.

  Could you describe the general scheme to define `mux2^n` (a) using only boolean operations (b) using only `mux2^(n-1)` (c) using only `mux2`?
]
#exercise(sub: "Modular Exponentiation")[
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
  Write a function `squareCount :: Int -> Int` to count number of squares on a  $n times n$ grid. For example, `squareCount 1 = 1` and `squareCount 2 = 5` as four 1 $times$ 1 squares and one 2x2 square.

  Furthermore, also make a function `rectCount :: Int -> Int` to count the number of rectangles on a $n times n$ grid.

  Finally, make `genSquareCount :: (Int, Int) -> Int` and `genRectCount :: (Int, Int) -> Int` to count number of squares and rectangle in a $a times b$ grid.
]

#exercise(sub:"Knitting Baltik (COMPFEST 13, Codeforces 1575K)")[
Mr. Chanek wants to knit a batik, a traditional cloth from Indonesia. The cloth forms a grid with size $m times n$. There are $k$
colors, and each cell in the grid can be one of the $k$ colors.

Define a sub-rectangle as an pair of two cells $((x_1,y_1), (x_2,y_2))$, denoting the top-left cell and bottom-right cell (inclusively) of a sub-rectangle in the grid.
Two sub-rectangles $((x_1,y_1), (x_2,y_2))$ and $((x_1,y_1), (x_2,y_2))$ have the same pattern if and only if the following holds:

(i) they have the same width ($x_2 - x_1 = x_4 - x_3$);

(ii) they have the same height ($y_2 - y_1 = y_4 - y_3$);

(iii) for every pair $i,j$ such that $0 <= i <= x_2 - x_1$ and $0 <= j <= y_2 - y_1$, the color of cells $(x_1 + i, y_1 + j)$ and $(x_3 + i, y_3 + j)$ is the same.

Write a function `countBaltik` of type \ 
`(Int, Int) -> Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Integer` to count the number of possible batik color combinations, such that the subrectangles $((a_x,a_y),(a_x+r-1,a_y + c - 1))$ and $((b_x,b_y),(b_x+r-1,b_y + c - 1))$ have the same pattern.

*Input*
`countBaltik` takes as input:

- The size of grid $(m, n)$
- Number of colors $k$
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

  Thr problem with this remarkable formula is that it is cluttered with irrational numbers, specifically $sqrt(5)$.
  $
    F_n = ((1+sqrt(5))^n - (1-sqrt(5))^n)/(2^n sqrt(5))
  $

  While computing using the Binet formula would only take $2 * log(n) + 2$ operations (exponentiation takes $log(n)$ time), doing so directly is out of the question as we can't represent $sqrt(5)$ exactly and the small errors in the approximation will accumulate due to the number of operations.
  
  So an idea is to do all computations on a tuple $(a,b)$ which represents $a + b sqrt(5)$, We will need to define *addition, subtraction, multiplication and division* on these tuples as well as define a *fast exponentiation* here. 

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

  You can see the pattern as well as the extreme growth rate. Make a function `knuthArrow :: Integer -> Int -> Integer -> Integer` which takes the first argument, number of arrows and second argument and provides the answer.
]

#exercise(sub : "Caves (IOI 2013, P4)")[
While lost on the long walk from the college to the UQ Centre, you have stumbled across the entrance to a secret cave system running deep under the university. The entrance is blocked by a security system consisting of $N$ consecutive doors, each door behind the previous; and $N$ switches, with each switch connected to a different door.

The doors are numbered $0, 1, dots, 4999$ in order, with door $0$ being closest to you. The switches are also numbered $0, 1, dots, 4999$, though you do not know which switch is connected to which door.

The switches are all located at the entrance to the cave. Each switch can either be in an up or down position. Only one of these positions is correct for each switch. If a switch is in the correct position then the door it is connected to will be open, and if the switch is in the incorrect position then the door it is connected to will be closed. The correct position may be different for different switches, and you do not know which positions are the correct ones.

You would like to understand this security system. To do this, you can set the switches to any combination, and then walk into the cave to see which is the first closed door. Doors are not transparent: once you encounter the first closed door, you cannot see any of the doors behind it. You have time to try $70,000$ combinations of switches, but no more. Your task is to determine the correct position for each switch, and also which door each switch is connected to.
]

#exercise(sub : "Carnivel (CEIO 2014)")[
Each of Peter’s $N$ friends (numbered from $1$ to $N$) bought exactly one carnival costume in order to wear it at this year’s carnival parties. There are $C$ different kinds of costumes,numbered from $1$ to $C$. Some of Peter’s friends, however, might have bought the same kind of costume. Peter would like to know which of his friends bought the same costume. For this purpose, he organizes some parties, to each of which he invites some of his friends. 

Peter knows that on the morning after each party he will not be able to recall which costumes he will have seen the night before, but only how many different kinds of costumes he will have seen at the party. Peter wonders if he can nevertheless choose the guests of each party such that he will know in the end, which of his friends had the same kind of costume. Help Peter!

Peter has $N <= 60$ friends and we can not have more than $365$ parties(as we want to know the costumes by the end of the year).
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