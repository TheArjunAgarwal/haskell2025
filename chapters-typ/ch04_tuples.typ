#metadata[
```haskell top
module TypesAsSets where
```
]

#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Proof.typ" : proof

#metadata[
```haskell
class (Enum t, Bounded t) => Finite t

listOfAllElements :: Finite t => [t]
listOfAllElements = [ minBound .. maxBound ]

data X = X1 | X2 | X3 deriving (Enum, Bounded, Show)
instance Finite X

data Y = Y1 | Y2      deriving (Enum, Bounded, Show)
instance Finite Y

instance Finite Bool

instance Finite Char

instance Finite ()

instance Finite Int
```
]

= Sets <sets>

#def(sub: "set" )[
  A *set* is a _well-defined collection of "things"_. \
  These "things" can be values, objects, or other sets. \ 
  For any given set, the "things" it contains are called its *elements*.
]

Some basic kinds of sets are -
- #def(sub: "empty set" )[
  The *empty set* is the _set that contains no elements_ or equivalently, _${}$_.
]
- #def(sub: "singleton set" )[ A *singleton set* is a _set that contains exactly one element_, such as ${34}, {triangle}$, the set of natural numbers strictly between $1$ and $3$, etc. ]

We might have encountered some mathematical sets before, such as the set of real numbers $RR$ or the set of natural numbers $NN$, or even a set following the rules of vectors ( a vector space ).

We might have encountered sets as data structures acting as an unordered collection of objects or values, such as Python sets - ```py set([])```,```py {1,2,3}```, etc.

Note that sets can be finite ( ${12,1,compose,arrow(x)}$ ),  as well as infinite ( $NN$ ) .

A fundamental keyword on sets is "$in$", or "belongs".
#def(sub: "belongs")[
  Given a value $x$ and a set $S$, \ 
  *$x in S$* is a _claim_ that _$x$ is an element of $S$_,  
]

Other common operations include - 

#def(sub: "union" )[
  *$A union B$* is the _set containing all those $x$ such that either $x in A$ or $x in B$_.
]

#def(sub: "intersection" )[
  *$A inter B$* is  the _set containing all those $x$ such that $x in A$ and $x in B$_.
]

#def(sub: "cartesian product" )[
  *$A times B$* is the _set containing all ordered pairs $(a,b)$ such that $a in A$ and $b in B$_.
]

So, 
$
X == { x_1 , x_2 , x_3 } "and" Y == { y_1 , y_2 } \ => \ X times Y ==  { (x_1,y_1),(x_1,y_2),(x_2,y_1),(x_2,y_2),(x_3,y_1),(x_3,y_2) }
$

#def(sub: "set exponent" )[
  *$B^A$* is the _set of all functions with domain $A$ and co-domain $B$_, \
  or equivalently , the _set of all functions $f$ such that $f:A->B$_, \
  or equivalently , the _set of all functions from $A$ to $B$_.
]

#exercise(sub: "size of exponent set" )[
  If $A$ has $|A|$ elements, and $B$ has $|B|$ elements, then how many elements does $B^A$ have?
]

= Types <types>

We have encountered a few types in the previous chapter, such as `Bool`, `Integer` and `Char`. For our limited purposes, we can think about each such *type* as the *set of all values of that type*.

For example, 
- `Bool` can be thought of as the *set of all boolean values*, which is {`False`, `True`}.
- `Integer` can be thought of as the *set of all integers*, which is {`0`, `1`, `-1`, `2`, `-2`, . . . }.
- `Char` can be thought of as the *set of all characters*, which is {`'\NUL'`,`'\SOH'`,`'\STX'`, . . . ,`'a'`,`'b'`,`'c'`, . . . ,`'A'`,`'B'`,`'C'`, . . . }

If this analogy were to extend further, we might expect to see analogues of the basic kinds of sets and the common set operations for types, which we can see in the following -

== `::` is analogous to $in$ or @definition_of_belongs

Whenever we want to claim a value `x` is of type `T`, we can use the `::` keyword, in a similar fashion to $in$, i.e., we can say `x::T` in place of $x in T$.

In programming terms, this is known as declaring the variable `x`.

For example, 

- #[
```haskell
-- | declaration of x
x :: Integer
x = 42
``` ] This reads - "Let $x in ZZ$. Take the value of $x$ to be $42$."
- #[
```haskell
-- | declaration of y
y :: Bool
y = xor True False
```] This reads - "Let $y in$ {False, True}. Take the value of $y$ to be the $xor$ of True and False."

#metadata[
```haskell
    where xor = (/=)
```
]

#exercise(sub: "declaring a variable" )[
  Declare a variable of type `Char`.
]

== `A -> B` is analogous to $B^A$ or @definition_of_set_exponent

As $B^A$ contains all functions from $A$ to $B$, \ so is each function `f` defined to take an input of type `A` and output of type `B` satisfy `f::A->B`.

For example -

- #[
```
-- | function
succ :: Integer -> Integer
succ x = x + 1
```]

- #[
```
-- | another function
even :: Integer -> Bool
even n = if n `mod` 2 == 0 then True else False
```]

#exercise(sub: "basic function definition" )[
  Define a non-constant function of type `Bool -> Integer`.
]

#exercise(sub: "difference between declaration and function definition")[
  What are the differences between declaring a variable and defining a function?
]

== `( A , B )` is analogous to $A times B$ or @definition_of_cartesian_product

As $A times B$ contains all pairs $(a,b)$ such that $a in A$ and $b in B$, \ so is every pair `(a,b)` of type `(A,B)` if `x` is of type `A` and `b` is of type `B`.

For example, if I ask GHCi to tell me the type of `(True, 'c')`, then it would tell me that the value's type is `(Bool, Char)` -
```
-- | type of a pair
>>> :type (True, 'c')
(True, 'c') :: (Bool, Char)
``` This reads - "GHCi, what is the type of `(True, 'c')`? \ #v( 0.1em , weak: true)
$"                "$Answer : the type of `(True, 'c')` is `(Bool, Char)`."

#metadata[
```haskell
instance (Finite a , Finite b ) => Enum (a,b) where

    toEnum :: (Finite a , Finite b ) => Int -> (a, b)
    toEnum = (!!) $ (,) <$> listOfAllElements <*> listOfAllElements

    fromEnum :: (Finite a , Finite b ) => (a, b) -> Int
    fromEnum ( a , b ) = fromEnum a * length ( listOfAllElements :: [ b ] ) + fromEnum b 

instance ( Finite a , Finite b ) => Finite (a,b)
```
]

If we have a type `X` with elements `X1`, `X2`, and `X3`, and another type `Y` with elements `Y1` and `Y2`, we can use the author-defined function `listOfAllElements` to obtain a list of all elements of certain types -
```
-- | elements of a product type
>>> listOfAllElements :: [X]
[X1,X2,X3]

>>> listOfAllElements :: [Y]
[Y1,Y2]

>>> listOfAllElements :: [(X,Y)]
[(X1,Y1),(X1,Y2),(X2,Y1),(X2,Y2),(X3,Y1),(X3,Y2)]

>>> listOfAllElements :: [(Char,Bool)]
[('\NUL',False),('\NUL',True),('\SOH',False),('\SOH',True), . . . ]
```

There are two fundamental inbuilt operations from a product type -

A function to get the first component of a pair - 
```
-- | first component of a pair
fst (a,b) = a
```
and a similar function to get the second component -
```
-- | second component of a pair
snd (a,b) = b
```

We can define our own functions from a product type using these -
```haskell
-- | function from a product type
xorOnPair :: ( Bool , Bool ) -> Bool
xorOnPair pair = ( fst pair ) /= ( snd pair )
```
or even by pattern matching the pair -
```haskell
-- | another function from a product type
xorOnPair' :: ( Bool , Bool ) -> Bool
xorOnPair' ( a , b ) = a /= b
```

Also, we can define our functions to a product type - \
For example, consider the useful inbuilt function `divMod`, which *divides a number by another*, and *returns* both the *quotient and the remainder as a pair*.
Its definition is equivalent to the following -
```
-- | function to a product type
divMod :: Integer -> Integer -> ( Integer , Integer )
divMod n m = ( n `div` m , n `mod` m )
```

#exercise(sub: "size of a product type" )[
  If a type `T` has $n$ elements, and type `T'` has $m$ elements, then how many elements does `(T.T')` have?
]

== `()` is analogous to @definition_of_singleton_set

`()`, pronounced Unit, is a type that contains exactly one element. \ That unique element is `()`.

So, it means that `()::()`, which might appear a bit confusing.

The `()` on the left of `::` is just a simple value, like `1` or `'a'`. \
The `()` on the right of `::` is a type, like `Integer` or `Char`.

This value `()` is the only value whose type is `()`.

On the other hand, other types might have multiple values of that type. (such as `Integer`, where both `1` and `2` have type `Integer`.)

We can even check this using `listOfAllElements` - 
```
-- | elements of unit type
>>> listOfAllElements :: [()]
[()]
```
This reads - "The list of all elements of the type `()` is a list containing exactly one value, which is the value `()`."

#exercise(sub: "function to unit")[
  Define a function of type `Bool -> ()`.
]

#exercise(sub: "function from unit")[
  Define a function of type `() -> Bool`.
]

== No @definition_of_intersection of Types 

We now need to discuss an important distinction between sets and types.
While two different sets can have elements in common, like how both $RR$ and $NN$ have the element $10$ in common, on the other hand, two different types `T1` and `T2` cannot have any common elements.

//todo explanation in appendix

For example, the types `Int` and `Integer` have no elements in common. We might think that they have the element `10` in common, however, the internal structures of `10::Int` and `10::Integer` are very different, and thus the two `10`s are quite different.

Thus, the intersection of two different types will always be empty and doesn't make much sense anyway. 

Therefore, no intersection operation is defined for types.

== No @definition_of_union of Types 

Suppose the type `T1`$union$`T2` were an actual type. It would have elements in common with the type `T1`. As discussed just previously, this is undesirable and thus disallowed.

But there is a promising alternative, for which we need to define the set-theoretic notion of *disjoint union*.

#exercise(sub: "subtype" )[
  Do you think that there can be an analogue of the _subset_ relation $subset.eq$ for types?
]

== Disjoint Union of Sets

#def(sub: "disjoint union" )[
  *$A union.sq B$* is defined to be _$( {0} times A ) union ({1} times B)$_,
  or equivalently, _the set of all pairs either of the form $(0,a)$ such that $a in A$, or of the form $(1,b)$ such that $b in B$_.
]

So, 
$
X == { x_1 , x_2 , x_3 } "and" Y == { y_1 , y_2 } \ => \ X union.sq Y == { (0,x_1) , (0,x_2) , (0,x_3) , (1,y_1) , (1,y_2) }
$

The main advantage that this construct offers us over the usual @definition_of_union is that given an element $x$ from a disjoint union $A union.sq B$, it is very easy to see whether $x$ comes from $A$, or whether it comes from $B$.

For example, consider the statement - $(0,10) in RR union.sq NN$. \ 
It is obvious that this "$10$" comes from $RR$ and does not come from $NN$.\ 
$(1,10) in RR union.sq NN $ would indicate exactly the alternative, i.e, the "$10$" here comes from $NN$, not $RR$.

== `Either A B` is analogous to $A union.sq B$ or @definition_of_disjoint_union

The term "either" is motivated by its appearance in the definition of @definition_of_disjoint_union.

Recall that in a @definition_of_disjoint_union , each element has to be
- of the form $(0,a)$, where $a in A$, and $A$ is the set to the left of the $union.sq$ symbol, 
- or they can be of the form $(1,b)$, where $b in B$, and $B$ is the set to the right of the $union.sq$ symbol.

Similarly, in `Either A B`, each element has to be
- of the form `Left a`, where `a::A`
- or of the form `Right b`, where `b::B`

#metadata[
```haskell
instance ( Finite a , Finite b ) => Enum (Either a b) where

    toEnum :: ( Finite a , Finite b ) => Int -> Either a b
    toEnum = (!!) $ (++) ( Left <$> listOfAllElements ) ( Right <$> listOfAllElements )

    fromEnum :: ( Finite a , Finite b ) => Either a b -> Int
    fromEnum ( Left a ) = fromEnum a
    fromEnum ( Right b ) = length ( listOfAllElements :: [ a ] ) + fromEnum b

instance ( Finite a , Finite b ) => Bounded (Either a b) where

    minBound :: ( Finite a , Finite b ) => Either a b
    minBound = Left minBound
    
    maxBound :: ( Finite a , Finite b ) => Either a b
    maxBound = Right maxBound

instance ( Finite a , Finite b ) => Finite (Either a b)
```
]

If we have a type `X` with elements `X1`, `X2`, and `X3`, and another type `Y` with elements `Y1` and `Y2`, we can use the author-defined function `listOfAllElements` to obtain a list of all elements of certain types -
```
-- | elements of an either type
>>> listOfAllElements :: [X]
[X1,X2,X3]

>>> listOfAllElements :: [Y]
[Y1,Y2]

>>> listOfAllElements :: [Either X Y]
[Left X1,Left X2,Left X3,Right Y1,Right Y2]

>>> listOfAllElements :: [Either Bool Char]
[Left False,Left True,Right '\NUL',Right '\SOH',Right '\STX', . . . ]
```

We can define functions to an `Either` type. \
Consider the following problem : We have to make a function that provides feedback on a quiz. We are given the marks obtained by a student in the quiz marked out of 10 total marks. If the marks obtained are less than 3, return `'F'`, otherwise return the marks as a percentage -
```haskell
-- | function to an either type
feedback :: Integer -> Either Char Integer
--                     Left ~ Char,Integer ~ Right
feedback n
  | n < 3     = Left  'F'
  | otherwise = Right ( 10 * n ) -- multiply by 10 to get percentage
```
This reads - "

Let `feedback` be a function that takes an `Integer` as input and returns `Either` a `Char` or an `Integer`. \

As `Char` and `Integer` occurs on the left and right of each other in the expression `Either Char Integer`, thus `Char` and `Integer` will henceforth be referred to as `Left` and `Right` respectively. 

Let the input to the function `feedback` be `n`.

If `n<3`, then we return `'F'`. To denote that `'F'` is a `Char`, we will tag `'F'` as `Left`. (remember that `Left` refers to  `Char`!)

`otherwise`, we will multiply `n` by `10` to get the percentage out of 100 (as the actual quiz is marked out of 10). To denote that the output `10*n` is an `Integer`, we will tag it with the word `Right`. (remember that `Right` refers to `Integer`!)

"

We can also define a function from an `Either` type. \
Consider the following problem : We are given a value that is either a boolean or a character. We then have to represent this value as a number. 
```haskell top
import Data.Char(ord)
```
```haskell
-- | function from an either type
representAsNumber :: Either Bool Char -> Int
--                   Left ~ Bool,Char ~ Right
representAsNumber ( Left  bool ) = if bool then 1 else 0
representAsNumber ( Right char ) = ord char
```
This reads - "

Let `representAsNumber` be a function that takes either a `Bool` or a `Char` as input and returns an `Int`. \

As `Bool` and `Char` occurs on the left and right of each other in the expression `Either Bool Char`, thus `Bool` and `Char` will henceforth be referred to as `Left` and `Right` respectively. 

If the input to `representAsNumber` is of the form `Left bool`, we know that `bool` must have type `Bool` (as `Left` refers to `Bool`). So if the `bool` is `True`, we will represent it as `1`, else if it is `False`, we will represent it as `0`.

If the input to `representAsNumber` is of the form `Right char`, we know that `char` must have type `Char` (as `Right` refers to `Char`). So we will represent `char` as `ord char`.

"

We might make things clearer if we use a deeper level of pattern matching, like in the following function ( which is equivalent to the last one ).
```haskell
-- | another function from an either type
representAsNumber' :: Either Bool Char -> Int
representAsNumber' ( Left  False ) = 0
representAsNumber' ( Left  True  ) = 1
representAsNumber' ( Right char  ) = ord char
```

#exercise(sub: "size of an either type" )[
  If a type `T` has $n$ elements, and type `T'` has $m$ elements, then how many elements does `Either T T'` have?
]

== The `Maybe` Type <maybe>

Consider the following problem : We are asked make a function `reciprocal` that reciprocates a rational number, i.e., $( x |-> 1/x ) : QQ -> QQ$.

Sounds simple enough! Let's see -
```
-- | naive reciprocal
reciprocal :: Rational -> Rational
reciprocal x = 1/x
```
But there is a small issue! What about $1/0$? \
What should be the output of `reciprocal 0`?

Unfortunately, it results in an error - 
```
>>> reciprocal 0
*** Exception: Ratio has zero denominator
```

To fix this, we can do something like this -
Let's add one _extra element_ to the output type `Rational`, and then `reciprocal 0` can have this _extra element_ as its output!

So the new output type would look something like this - $({$_extra element_$}union.sq$`Rational`$)$

Notice that this ${$_extra element_$}$ is a @definition_of_singleton_set. \ Which means that if we take this _extra element_ to be the value `()`, \  and take ${$_extra element_$}$ to be the type `()`, \ 
then we can obtain $({$_extra element_$}union.sq$`Rational`$)$ as the type `Either () Rational`.

Then we can finally rewrite @code_of_naive_reciprocal to handle the case of `reciprocal 0` - 
```
-- | reciprocal using either
reciprocal :: Rational -> Either () Rational
reciprocal 0 = Left  ()
reciprocal x = Right (1/x)
```

There is already an inbuilt way to express this notion of `Either () Rational` in Haskell, which is the type `Maybe Rational`. 

`Maybe Rational` just names it elements a bit differently compared to `Either () Rational` -
- where \ `Either () Rational` has `Left ()`, \ `Maybe Rational` instead has the value `Nothing`.
- where \ `Either () Rational` has `Right r` (where `r` is any `Rational`), \ `Maybe Rational` instead has the value `Just r`.

Which means that we can rewrite @code_of_reciprocal_using_either using `Maybe` instead -
```haskell
-- | function to a maybe type
reciprocal :: Rational -> Maybe Rational
reciprocal 0 = Nothing
reciprocal x = Just (1/x)
```

But we can also do this for any arbitrary type `T` in place of `Rational`. In that case - 

There is already an inbuilt way to express the notion of `Either () T` in Haskell, which is the type `Maybe T`. 

`Maybe T` just names it elements a bit differently compared to `Either () T` -
- where \ `Either () T` has `Left ()`, \ `Maybe T` instead has the value `Nothing`.
- where \ `Either () T` has `Right t` (where `t` is any value of type `T`), \ `Maybe T` instead has the value `Just t`.

#metadata[
```haskell
instance Finite t => Enum (Maybe t) where

    toEnum :: Finite t => Int -> Maybe t
    toEnum 0 = Nothing
    toEnum n = Just $ toEnum ( n - 1 )

    fromEnum :: Finite t => Maybe t -> Int
    fromEnum Nothing = 0
    fromEnum (Just t) = 1 + fromEnum t

instance Finite t => Bounded (Maybe t) where

    minBound :: Finite t => Maybe t
    minBound = Nothing

    maxBound :: Finite t => Maybe t
    maxBound = Just maxBound

instance Finite t => Finite ( Maybe t )
```
]

If we have a type `X` with elements `X1`, `X2`, and `X3`, and another type `Y` with elements `Y1` and `Y2`, we can use the author-defined function `listOfAllElements` to obtain a list of all elements of certain types -
```
-- | elements of a maybe type
>>> listOfAllElements :: [X]
[X1,X2,X3]

>>> listOfAllElements :: [Maybe X]
[Nothing,Just X1,Just X2,Just X3]

>>> listOfAllElements :: [Y]
[Y1,Y2]

>>> listOfAllElements :: [Maybe Y]
[Nothing,Just Y1,Just Y2]

>>> listOfAllElements :: [Maybe Bool]
[Nothing,Just False,Just True]

>>> listOfAllElements :: [Maybe Char]
[Nothing,Just '\NUL',Just '\SOH',Just '\STX',Just '\ETX', . . . ]
```

#exercise(sub: "size of a maybe type" )[
  If a type `T` has $n$ elements, then how many elements does `Maybe T` have?
]

We can define functions to a `Maybe` type.
For example consider the problem of making an inverse function of `reciprocal`, i.e., a function `inverseOfReciprocal` s.t. #align(center)[$forall$` x::Rational `,` inverseOfReciprocal ( reciprocal x ) == x `] as follows -
```haskell
-- | function from a maybe type
inverseOfReciprocal :: Maybe Rational -> Rational
inverseOfReciprocal Nothing  = 0
inverseOfReciprocal (Just x) = (1/x)
```

== `Void` is analogous to ${}$ or @definition_of_empty_set

The type `Void` has no elements at all.

This also means that no actual value has type `Void`.

Even though it is out-of-syllabus, an interesting exercise is to #exercise[try to define a function of type `( Bool -> Void ) -> Void`.]

= Currying 

Let's try to explore some more elaborate types. \ For example, let us try to find out the *type of the derivative operator*, $ d/(d x) $

Let $DD$ be the set of all differentiable $RR -> RR$ functions.

Now, for any $f in DD$, i.e., for any differentiable function $f : RR -> RR$,\ we know that $(d f)/(d x)$ will be also be a $RR -> RR$ function. 

Specifically, we could define
$
  (d f)/(d x) := ( p |-> lim_(h -> 0)(f(p+h)-f(p))/h)
$

Therefore, the function $d/(d x)$\ takes an input $f$ of type $DD$, \ and produces an output $(d f)/(d x)$ of type $RR -> RR$, which is written set-theoretically as $RR^RR$.

An thus we obtain the type of the derivative operator as
$
  d/(d x) : DD -> ( RR -> RR )
$
or more formally,
$
  d/(d x) : DD -> RR^RR
$

But we know another syntax for writing the derivative, which is - $ (d f)/(d x)#h(0em,weak:true)#text(size:2.1em,baseline:0.1em)[$bar$] #h(0pt,weak:true) #text(size:1em,baseline:0.8em)[$p$] $, which refers to the derivative evaluated at a point $p in RR$.

Here, the definition could be written as
$
  (d f)/(d x)#h(0em,weak:true)#text(size:2.1em,baseline:0.1em)[$bar$] #h(0pt,weak:true) #text(size:1em,baseline:0.8em)[$p$] := lim_(h -> 0)(f(p+h)-f(p))/h
$

So here there are two inputs, namely $f in DD$ and $p in RR$, \
and an output $(d f)/(d x)#h(0em,weak:true)#text(size:1.6em,baseline:0.1em)[$bar$] #h(0pt,weak:true) #text(size:0.8em,baseline:0.6em)[$p$]$, which is of type $RR$.

That leads us to the type -
$  
  d/(d x) : DD times RR -> RR
$

We understand that these two definitions are equivalent.\
So now the question is, which type do we use?

High-school math usually chooses to use the $DD times RR -> RR$ style of typing.

Haskell, and in several situations math as well,\ defaults to the $DD -> ( RR -> RR ) $, or equivalently $DD ->  RR^RR$ style of typing.

#box(stroke:yellow, inset:0.7em, width:100%)[
  In general, that means that if a function $F : A -> ( B -> C ) $ 

  takes an input from $A$, \ and gives as output a $B -> C$ function,

  then it is equivalent to saying $F : A times B -> C$, which would make $F$ a function 

  that takes inputs of type $A$ and $B$ respectively, \ and gives an output of type $C$.
]#footnote([will be proven soon])

We have just seen the example where $F$ was $d/(d x)$ and $A,B,C$ were $DD,RR,RR$ respectively.

However, this has more profound consequences than what appears at first glance, in Haskell as well as in post-high-school mathematics.

This is due to looking in the opposite direction, i.e., taking a definition like 
$
  (d f)/(d x)#h(0em,weak:true)#text(size:2.1em,baseline:0.1em)[$bar$] #h(0pt,weak:true) #text(size:1em,baseline:0.8em)[$p$] := lim_(h -> 0)(f(p+h)-f(p))/h
$
and rephrasing it as
$
  (d f)/(d x) := ( p |-> lim_(h -> 0)(f(p+h)-f(p))/h)
$

#box(stroke:yellow, inset:0.7em, width:100%)[
  In general, if a function $F : A times B -> C$

  takes inputs of type $A$ and $B$ respectively, \ and gives an output of type $C$.

  then it is equivalent to saying $F : A -> ( B -> C )$, which would make $F$ a function 

  that takes an input from $A$, \ and gives as output a $B -> C$ function.

]#footnote([will be proven soon])

This rephrasing is known as "currying".

== In Haskell

Again, for example,\ if we have a function such as $d/(d x)$ which has *2 inputs* ($f$ and $p$),
\ we can use it by *only giving the first input*\ in the following sense -
$
  (d f)/(d x) := ( p |-> (d f)/(d x)#h(0em,weak:true)#text(size:2.1em,baseline:0.1em)[$bar$] #h(0pt,weak:true) #text(size:1em,baseline:0.8em)[$p$])
$

Let's see how it works in Haskell.

#def(sub:"currying rule")[If we have a function `f` that takes 2 inputs (say `x` and `y`), then we can use `f x` as
```
f x = \ y -> f x y
```]

We know that `(+)` is a function that takes in two `Integer`s and outputs an `Integer`. \ This means that $A,B,C$ are `Integer`,`Integer`,`Integer` respectively.

By currying or rephrasing, this would mean that we could treat `(+)` like a function that takes a single input of type `Integer` (i.e., $A$) and outputs a function of type `Integer -> Integer` (i.e., $B -> C$).

In fact, that's exactly what Haskell lets you do - 
```
>>> :type +d (+) 17    
(+) 17 :: Integer -> Integer
```

Meaning that when `(+)` is given the `Integer` input `17`, it outputs the function `(+) 17`, of type `Integer -> Integer`.

More explicitly, by the @definition_of_currying_rule, we have that
```
(+) 17 = \ y -> (+) 17 y
```

Thus, what does this function `(+) 17` actually do?\
Simple! It is a function that takes in any `Integer` and adds `17` to it.

So, for example,\
If we define -
```haskell
-- | currying usage
test = (+) 17
```
it behaves as such -
```
>>> test 0
17
>>> test 1
18
>>> test 12
29
>>> test (-17)
0
```

Another - 

```
>>> :type +d (*)
(*) :: Integer -> Integer -> Integer

>>> :type +d (*) 2
(*) 2 :: Integer -> Integer
```

Meaning that when `(*)` is given the `Integer` input `2`, it outputs the function `(*) 2`, of type `Integer -> Integer`.

More explicitly, by the @definition_of_currying_rule, we have that
```
(*) 2 = \ y -> (*) 2 y
```

Thus, the function `(*) 2`\ takes in an `Integer` input\ and multiplies it by `2`, 
i.e., doubles it.

So if we define -
```haskell
-- | another currying usage
doubling :: Integer -> Integer
doubling = (*) 2
```
it behaves as such -
```
>>> doubling 0
0
>>> doubling 1
2
>>> doubling 12
24
>>> doubling (-17)
-34
```

== Understanding through Associativity

=== Of `->`

The @definition_of_currying_rule essentially allows us to view a function of type `A -> B -> C` as of type `A -> ( B -> C )`.

This is due to the fact that as an @definition_of_infix_binary_operator, the `->` operator is @definition_of_right-associative.

Recalling the definition of @definition_of_right-associative, this means that,\ for any `X`,`Y`,`Z` -
```
X -> Y -> Z
```
is actually equivalent to
```
X -> ( Y -> Z )
```

And thus the @definition_of_currying_rule is justified.

=== Of Function Application

Let us take the @definition_of_currying_rule
```
f x = \ y -> f x y
```
Applying a few transformations to both sides - 
```
( f x ) == ( \ y -> f x y )
-- applying both sides to y
( f x ) y == ( \ y -> f x y ) y
-- simplifying
( f x ) y == f x y
-- exchanging LHS and RHS
f x y == ( f x ) y
```
Thus we obtain the result that any time we write 
```
f x y
```
it is actually equivalent to
```
( f x ) y
```

This means that "function application" is @definition_of_left-associative. (Recall the definition of @definition_of_left-associative and see if this makes sense)

That is, if we apply a function `f` to 2 inputs `x`
and `y` in the form `f x y`, \
then `f x` (the application on the *left*) is evaluated first (as seen in `( f x ) y`) and then the obtained `( f x )` is applied on `y`.

=== Operator Currying Rule

We have already seen the @definition_of_currying_rule. However it can be extended in a special way when the function is an @definition_of_infix_binary_operator.

#def(sub:"operator currying rule")[
  If we have an @definition_of_infix_binary_operator `⨝`, then we can assume the following due to the @definition_of_currying_rule - 
  ```
  (⨝) x = \ y -> (⨝) x y -- the normal currying rule
  -- which is equivalent to
  (⨝) x = \ y -> x ⨝ y
  ```
  But we may further assume
  ```
  (x⨝) = \ y -> x ⨝ y
  ```
  and also
  ```
  (⨝y) = \ x -> x ⨝ y
  
  ```
  This means that while the @definition_of_currying_rule allowed us to give only the _first input_ (i.e.,`x`) and get a meaningful function out of it,\
  the *operator currying rule* further allows to do something similar by only giving the _second input_ (i.e.,`y`).
]

For example, -

```
>>> :type +d (^)
(^) :: Integer -> Integer -> Integer

>>> :type +d (^2)
(^2) :: Integer -> Integer
```

Meaning that when the @definition_of_infix_binary_operator `^` is given the `Integer` input `2` in place of its second input, it outputs the function `(^2)`, of type `Integer -> Integer`.

More explicitly, by the @definition_of_operator_currying_rule -
```
(^2) = \ x -> x ^ 2
```
Thus, `(^2)` is a function that takes an `Integer` `x` and raises to to the power of `2`, i.e., *squares* it.

So if we define - 
```haskell
-- | operator currying usage
squaring :: Integer -> Integer
squaring = (^2)
```
it will show the following behaviour - 
```
>>> squaring 0     
0
>>> squaring 1
1
>>> squaring 12
144
>>> squaring (-17)
289
```

For another example, we can define
```haskell
-- | another operator currying usage
cubing :: Integer -> Integer
cubing = (^3)
```
which works quite similarly.

== Proof of the Currying Theorem

What follows is an *OPTIONAL* formal rigorous proof of the following statement - 
#box(stroke:yellow, inset:0.7em, width:100%)[
  In general, if a function $F : A times B -> C$

  takes inputs of type $A$ and $B$ respectively, \ and gives an output of type $C$.

  then it is equivalent to saying $F : A -> ( B -> C )$, which would make $F$ a function 

  that takes an input from $A$, \ and gives as output a $B -> C$ function.

]

What we are going to prove is that \ there exists a bijection \ from 
\ the set ${ F | F : A times B -> C }$
\ to
\ the set ${ G | G : A -> ( B -> C ), "or equivalently",  G : A -> C^B}$

Note that the set ${ F | F : A times B -> C }$ can be expressed as $C^(A times B)$ \ and that the set ${ G | G : A -> C^B}$ can be expressed as $(C^B)^A$

Therefore, we have to prove there exists a bijection $: C^(A times B) -> (C^B)^A$

#exercise(sub:"finite currying")[Is there an easy way to prove the theorem in the case that $A,B,C$ are all finite sets?]

#pagebreak()

#proof(thm:[$exists$ a bijection $: C^(A times B) -> (C^B)^A$])[
  Define the function $cal(C)$ as follows -

  $
    cal(C) : C^(A times B) &-> (C^B)^A\
    cal(C)(F) &:= G\
  &"where"\
  &G : A -> C^B\
  &" "G(a) := (b |-> F(a,b))
  $
  If we can prove that $cal(C)$ is bijective, we are done!
  \ In order to do that, we will prove that $cal(C)$ is injective as well as a surjective.

  *Claim : * $cal(C)$ is injective
  \ *Proof : * 
  $
    &"                                        " cal(C)(F_1) &&== cal(C)(F_2)\
    &=> "                                         "G_1 &&== G_2 "   , where " G_1(a) := ( b |-> F_1(a,b) ) " and " G_2(a) := ( b |-> F_2(a,b) ) \
    &=> forall a in A,"                         " G_1(a) &&== G_2(a)", where " G_1(a) := ( b |-> F_1(a,b) ) " and " G_2(a) := ( b |-> F_2(a,b) ) \
    &=> forall a in A,"              " ( b |-> F_1(a,b) ) &&== ( b |-> F_2(a,b) )\
    &=> forall a in A, forall b in B, " " ( b |-> F_1(a,b) )(b) &&== ( b |-> F_2(a,b))(b)\
    &=> forall a in A, forall b in B, "            " F_1(a,b) &&== F_2(a,b)\
    &=> "  "forall p in A times B, "                 " F_1(p) &&==F_2(p)\
    &=> "                                         "F_1 &&==F_2
  $
  \
  *Claim : * $cal(C)$ is surjective
  \ *Proof : * Take an arbitrary $H in (C^B)^A$ .
  \ In other words, take an arbitrary function $H : A -> ( B -> C )$ .
  \ Define a function $J$ as follows -
  $
    J : A times B &-> C
    \ J(a,b) &:= (H(a))(b)
  $
  Now, 
  $
    cal(C)(J) &:= "  "G" , where " G(a) &&:= ( b |-> J(a,b) )
    \ &== G" , where " G(a) &&:= ( b |-> (H(a))(b) )
    \ &== G" , where " G(a) &&:= (H(a)) &&&[because (x|->f(x)) "is equivalent to just" f" "]
    \ &== G" , where " G &&== H &&&[because f(x):=g(x) "means that" f == g" "]
    \ &== H
  $
  That means we have proved that
  $
    forall H in (C^B)^A, exists J " such that " cal(C)(J) == H
  $
  Therefore, by the definition of surjectivity, we have proven that $cal(C)$ is surjective.
  \ As a result, we are done with the overall proof as well!
  \
  \

]
= Exercises
#exercise(sub : "Symmetric Difference")[
(i) Define the symmetric difference of the sets $A$ and $B$ as $A Delta B = (A backslash B) union (B backslash A)$.

Prove that this is a commutative and associative operation.

(ii) The set $A_1 Delta A_2 Delta dots Delta A_n$ consists of those elements that belong to an odd number of the $A_i$’s.
]
#exercise(sub : "Set of Size")[
  (i) Give a type with exactly $32$ elements.
  (ii) Give a type with exactly $108$ elements.
  (iii) Give a type with exactly $19$ elements.
]
#exercise(sub : "Unions and Intersections")[
  Prove:
  (i) $
  forall i in I, x_i subset.eq y => union.big_(i in I) x_i in y
  $

  (ii) $
  forall i in I, y subset.eq y => y in inter.big_(i in I) x_i
  $

  (iii) $
  union.big_(i in I) (x_i union y_i) = (union.big_(i in I) x_i) union (union.big_(i in I) y_i) 
  $

  (iv) $
  inter.big_(i in I) (x_i inter y_i) = (inter.big_(i in I) x_i) inter (inter.big_(i in I) y_i) 
  $

  (v) $
  union.big_(i in I) (x_i inter y) = (union.big_(i in I) x_i) inter y 
  $

  (vi) $
  inter.big_(i in I) (x_i union y) = (inter.big_(i in I) x_i) union y 
  $
]

#exercise(sub : "Flavoured like Curry")[
  $A tilde B$ means that there exists a bijection between $A$ and $B$.
  
  Prove:
  
  (i)$
  A (B + C) tilde A B + A C
  $

  (ii) $
  (B union C)^A tilde B^A times C^A$ provided $B inter C = emptyset
  $
  
  (iii) $
  C^(A times B) tilde C^A times C^B
  $
]

#exercise(sub:"Eckman-Hilton Argument")[
  #def(sub:"Unital Operator")[
    A binary operator $ast$ over a set $S$ is *Unital* if there exists $l, r in S op("s.t.") forall x in S, l ast x = x ast r = x$. (You can also prove $r = l$! This is why we normally label this $l = r = 1_S$ in abstract algebra.)
  ]

  If a set $S$ has two unital operations $star$ and $dot$ defined on it such that:
  $
  (w dot x) star (y dot z) = (w star x) dot (y star z)
  $
  Prove that $star equiv dot$.
]

#exercise(sub: "Associative Operators")[
  (i) How many different associative binary logical operators can be defined? (Formally, non-isomorphic associative binary logical operators. Informally, isomorphic means “same up to relabelling.”). Can you define all of them?

  (ii) Suppose we have an associative binary operator on a set $S$ of size $0 < k < oo$. Prove that $exists x in S, x dot x = x$.

  Note: A set with an associative binary operation is called a semi-group. The first question can also be posed for a set of general size, but we don't know the answer or an algorithm to get the answer beyond sets of size $9$.
]
#exercise(sub : "Feels Abstract")[
  #def(sub:"Shelf")[
  A set with an binary operation $gt.tri$ which left distributes over itself is called a left shelf, that is $forall x,y,z in S, x gt.tri (y gt.tri z) = (x gt.tri y) gt.tri (x gt.tri z)$.

  A similar definition holds for right shelf and the symbol often used is $lt.tri$.
  ]

  (i) Prove that a unital left shelf is associative. In other words: if there exists $1_S in S$ such that $1 gt.tri x = x gt.tri 1 = x$, then $gt.tri$ is associative.

  #def(sub:"Rack")[
    A rack is a set $R$ with two operations $gt.tri$ and $lt.tri$, such that R is a left shelf over $gt.tri$ and a right shelf over $lt.tri$ satisfying $x gt.tri (y lt.tri x) = (x gt.tri y) lt.tri x = y$.
  ]
  (ii) Prove that in a rack $R$, $gt.tri$ distributes over $lt.tri$ and vice versa. That is, $forall x,y,z in R; x gt.tri (y lt.tri z) = (x gt.tri y) lt.tri (x gt.tri z)$.

  (iii) We call an operator $star$ *idempotent* if $x star x = x$. If for a rack, $gt.tri$ is idempotent; prove that $lt.tri$ is idempotent.
  #def(sub: "Quandle")[
    A Quandle is a rack with $gt.tri$ and $lt.tri$ being idempotent.
  ]

  (iv) We call an operator $star$ *left involute* if $x star (x star y) = y$. Prove that an idempotent, left involutive, left shelf is a quandle (recall that the shelf only has one operator, we need to somehow suitably define the other operation. Maybe do problem (v) for a hint?)

  #def(sub:"Kei")[
    An involutive quandle is called a *Kei*
  ]

  (v) Prove that the set of points on the real plane with $x gt.tri y$ being the reflection of $y$ over $x$ is a Kei.

  Note: Shelves, Racks, Quandles and Kai's are slowly entering mainstream math as ways to work with operations on exotic sets like set of knots or set of colorings of primes etc. The theory of Kai in this regard was formalized very recently (2024) by Davis and Schlank in "Arithmetic Kei Theory". The main use is still Knot Theory, but we would not be surprised to see it used in a number theory proof. A reference for this, and a pre-req for Davis and Schlank, is "Quandles" by Mohamed Elhamdadi and Sam Nelson.
]

#exercise(sub : "Enumerating the Rationals")[
  (i) Prove that $ZZ$ is countable, that is there is a surjection from $NN -> ZZ$. #footnote[This may seem counterintuitive as the integers feel twice as large but they really are not]

  (ii) Now write functions : `natToInt :: Integer -> Integer` and `intToNat :: Integer -> Integer` which takes a natural number and gives the corresponding integer and vice versa.

  (iii) Prove that $NN times NN$ is countable, that is there is a bijection between $NN -> NN times NN$. While other arguments exist, we are big fans of enumerating in the order $(0,0) -> (0,1) -> (1,0) -> (0,2) -> (1,1) -> (2,0) -> dots$. What is the pattern?

  (iv) Now write a function `intToPair :: Int -> (Int, Int)` which takes an integer and gives the corresponding rational and a function `pairToInt :: (Int, Int) -> Int` which takes a pair and returns the corresponding integer.
]

#exercise(sub : "Calkin-Wilf Tree")[
    Using the above excercise,  we can see that $NN times NN$ is 'larger' #footnote[It is not as you will see in a moment,] than $QQ^+$, so we can claim that rationals are countable. We will attempt to prove that as well as enumerate the rationals.

    (i) Prove that if $p/q$ is reduced, then $(p+q)/q$ and $p/(p+q)$ are reduced.

    (ii) Prove that starting with $1/1$ and making the following tree by applying the above transformmation will contain every rational:
    #figure(image("../images/cwt.png"))

    (iii) Labeling the tree level by level, write a function `natToRat :: Int -> (Int, Int)` which takes an natural number and gives the positive rational enumerates. (An approach could be to notice that we can reprasent the path one takes down the tree in binary)

    (iv) Write a function `ratToNat :: (Int, Int) -> Int` which takes a positive rational number and gives its position in the naturals. 
]



