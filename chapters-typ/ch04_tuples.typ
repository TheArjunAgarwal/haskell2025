#metadata[
```haskell top
module TypesAsSets where
```
]

#import "../Definition.typ" : def
#import "../Exercise.typ" : exercise

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

= Sets

#def( subject : "set" )[
  A *set* is a _well-defined collection of "things"_. \
  These "things" can be values, objects, or other sets. \ 
  For any given set, the "things" it contains are called its *elements*.
]

Some basic kinds of sets are -
- #def( subject : "empty set" )[
  The *empty set* is the _set that contains no elements_ or equivalently, _${}$_.
]
- #def( subject : "singleton set" )[ A *singelten set* is a _set that contains exactly one element_, such as ${34}, {triangle}$, the set of natural numbers strictly betweeen $1$ and $3$, etc. ]

We might have encountered some mathematical sets before, such as the set of real numbers $RR$ or the set of natural numbers $NN$, or even a set following the rules of vectors ( a vector space ).

We might have encountered sets as data structures acting as an unordered collection of objects or values, such as Python sets - ```py set([])```,```py {1,2,3}```, etc.

Note that sets can be finite ( ${12,1,compose,arrow(x)}$ ),  as well as infinite ( $NN$ ) .

A fundamental keyword on sets is "$in$", or "belongs".
#def( subject : "belongs")[
  Given a value $x$ and a set $S$, \ 
  *$x in S$* is a _claim_ that _$x$ is an element of $S$_,  
]

Other common operations include - 

#def( subject : "union" )[
  *$A union B$* is the _set containing all those $x$ such that either $x in A$ or $x in B$_.
]

#def( subject : "intersection" )[
  *$A inter B$* is  the _set containing all those $x$ such that $x in A$ and $x in B$_.
]

#def( subject : "cartesian product" )[
  *$A crossmark B$* is the _set containing all ordered pairs $(a,b)$ such that $a in A$ and $b in B$_.
]

So, 
$
X == { x_1 , x_2 , x_3 } "and" Y == { y_1 , y_2 } \ => \ X crossmark Y ==  { (x_1,y_1),(x_1,y_2),(x_2,y_1),(x_2,y_2),(x_3,y_1),(x_3,y_2) }
$

#def( subject : "exponent" )[
  *$B^A$* is the _set of all functions with domain $A$ and co-domain $B$_, \
  or equivalently , the _set of all functions $f$ such that $f:A->B$_, \
  or equivalently , the _set of all functions from $A$ to $B$_.
]

#pagebreak()

= Types

We have enconutered a few types in the previous chapter, such as `Bool`, `Integer` and `Char`. For our limited purposes, we can think about each such *type* as the *set of all values of that type*.

For example, 
- `Bool` can be thought of as the *set of all boolean values*, which is {`False`, `True`}.
- `Integer` can be thought of as the *set of all integers*, which is {`0`, `1`, `-1`, `2`, `-2`, . . . },
- `Char` can be thought of as the *set of all characters*, which is {`'\NUL'`,`'\SOH'`,`'\STX'`, . . . ,`'a'`,`'b'`,`'c'`, . . . ,`'A'`,`'B'`,`'C'`, . . . }

If this analogy were to extend further, we might expect to see versions of the basic kinds of sets and the common set operations for types, as we can see in the following sections -

== `::` is analogous to $in$ or @definition_of_belongs

Whenever we want to claim a value `x` is of type `T`, we can use the `::` keyword, in a similar fashion to $in$, i.e., we can say `x::T` in place of $x in T$.

In programming terms, this is known as declaring the variable `x`.

For example, 

- ```haskell
-- | declaration of x
x :: Integer
x = 42
``` This reads - "Let $x in ZZ$. Take the value of $x$ to be $42$."

- ```haskell
-- | declaration of y
y :: Bool
y = xor True False
``` This reads - "Let $y in$ {False, True}. Take the value of $y$ to be the $xor$ of True and False."

#exercise( subject : "declaring a variable")[
  Declare a variable of type `Char`.
]

== `A -> B` is analogous to $B^A$ or @definition_of_exponent

As $B^A$ contains all functions from $A$ to $B$, \ so is each function `f` defined to take an input of type `A` and output of type `B` satisfy `f::A->B`.

For example -

- ```
-- | function
succ :: Integer -> Integer
succ x = x + 1
```

- ```
-- | another function
even :: Integer -> Bool
even n = if n `mod` 2 == 0 then True else False
```

#exercise( subject : "basic function definition")[
  Define a function of type `Bool -> Integer`.
]

#exercise( subject : " are function definitions declarations")[
  Are function definitions different from declarations?
]

== `( A , B )` is analogous to $A crossmark B$ or @definition_of_cartesian_product

As $A crossmark B$ contains all pairs $(a,b)$ such that $a in A$ and $b in B$, \ so is every pair `(a,b)` of type `(A,B)` if `x` is of type `A` and `b` is of type `B`.

For example, if I ask GHCi to tell me the type of `(True, 'c')` (which I can do using the command `:t`), then it would tell me that the value's type is `(Bool, Char)` -
```haskell
-- | type of a pair
>>> :t (True, 'c')
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
```haskell
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

#exercise( subject : "size of a product type")[
  If a type `T` has $n$ elements, and a type `T'` has $m$ elements, how many elements does `(T,T')` have?
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
-- | elements of the unit type
>>> listOfAllElements :: [()]
[()]
```
This reads - "The list of all elements of the type `()` is a list containing exactly one value, which is the value `()`."

#exercise( subject : "function from unit" )[
  Define a function of type `() -> Bool`.
]

#exercise( subject : "function to unit" )[
  Define a function of type `Bool -> ()`.
]

== No @definition_of_intersection of Types 

We now need to discuss an important distinction between sets and types.
While two different sets can have elements in common, like how both $RR$ and $NN$ have the element $10$ in common, on the other hand, two different types `T1` and `T2` cannot have any common elements. //todo explanation in appendix

For example, the types `Int` and `Integer` have no elements in common. We might think that they have the element `10` in common, however, the internal structures of `10::Int` and `10::Integer` are very different, and thus the two `10`s are quite different.

Thus, the intersection of two different types will always be empty and doesn't make much sense anyway. 

Therfore, no intersection operation is defined for types.

== No @definition_of_union of Types 

Suppose the type `T1`$union$`T2` were an actual type. It would have elements in common with the type `T1`. As discussed just previously, this is undesirable and thus disallowed.

But there is a promising alternative, for which we need to define the set-theoretic notion of *disjoint union*.

#exercise( subject : "subtypes" )[
  Can there be a analogue of the _subset_ relation or $subset.eq$ for types?
]

== Disjoint Union of Sets

#def( subject : "disjoint union" )[
  *$A union.sq B$* is defined to be _$( {0} crossmark A ) union ({1} crossmark B)$_,
  or equivalently, _the set of all pairs either of the form $(0,a)$ such that $a in A$, or of the form $(1,b)$ such that $b in B$_.
]

So, 
$
X == { x_1 , x_2 , x_3 } "and" Y == { y_1 , y_2 } \ => \ X union.sq Y == { (0,x_1) , (0,x_2) , (0,x_3) , (1,y_1) , (1,y_2) }
$

The main advantage that this construct offers us over the usual @definition_of_union is that given an element $x$ from a disjoint union $A union.sq B$, it is very easy to see whether $x$ comes from $A$, or whether it comes from $B$.

For example, consider the statement - $(0,10) in RR union.sq NN$. \ 
It is obvious that this $10$ comes from $RR$ and does not come from $NN$.\ 
$(1,10) in RR union.sq NN $ would indicate exactly the opposite, i.e, the $10$ here comes from $NN$, not $RR$.

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
Consider the folowing problem : We are given a value that is either a boolean or a character. We then have to represent this value as a number. 
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

If the input to `representAsNumber` is of the form `Right char`, we know that `char` must have type `Bool` (as `Right` refers to `Char`). So we will represent `char` as `ord char`.

"

We might make things clearer if we use a deeper level of pattern matching, like in the following function ( which is equivalent to the last one ).
```haskell
-- | another function from an either type
representAsNumber' :: Either Bool Char -> Int
representAsNumber' ( Left  False ) = 0
representAsNumber' ( Left  True  ) = 1
representAsNumber' ( Right char  ) = ord char
```

#exercise( subject : "size of an either type")[
  If a type `T` has $n$ elements, and a type `T'` has $m$ elements, how many elements does `Either T T'` have?
]

== The `Maybe` Type

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

We can also define functions from a `Maybe` type. \
For example, we can make a function that is the _inverse_ of `reciprocal`, i.e., a function `inverseOfReciprocal` such that #align(center)[$forall$` x::Rational `,` inverseOfReciprocal ( reciprocal x ) == x `] in the following way - 
```haskell
-- | function from a maybe type
inverseOfReciprocal :: Maybe Rational -> Rational
inverseOfReciprocal Nothing  = 0
inverseOfReciprocal (Just x) = (1/x)
```

#exercise( subject : "size of a maybe type")[
  If a type `T` has $n$ elements, how many elements does `Maybe T` have?
]

== `Void` is analogous to ${}$ or @definition_of_empty_set

The type `Void` has no elements at all.

This also means that no actual value has type `Void`.

Even though it is out-of-syllabus, an interesting exercise is to
#exercise[
  try to define a function of type `( Bool -> Void ) -> Void`.
]
