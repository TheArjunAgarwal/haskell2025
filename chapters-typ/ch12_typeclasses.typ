#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate

= Basics of Typeclasses

// == Methods

// Some functions can be used for multiple types, and so we have seen in @definition_of_Polymorphism.

// For example - 
// ```
// >>> :type id
// id :: a -> a
// ```
// And then we can -
// ```
// >>> id 12 -- use id as if id :: Integer -> Integer
// 12

// >>> id 'a' -- use id as if id :: Char -> Char
// 'a'

// >>> id False -- use id as if id :: Bool -> Bool
// False
// ```
// and 
// ```
// >>> id (\x->x*x)

// <interactive>:9:1: error: [GHC-39999]
//     * No instance for `Show (Integer -> Integer)'
//         arising from a use of `print'
//         (maybe you haven't applied a function to enough arguments?)
//     * In a stmt of an interactive GHCi command: print it
// ```
// Notice that even if it can
// Another example -
// ```
// >>> 11 == 12 -- use == as if (==) :: Integer -> Integer -> Bool
// False

// >>> 'a' == 'a'
// True

// >>> False == True
// False
// ```

== Vector Spaces

We will assume throughout that our *field of scalars* is $QQ$.\
And $QQ$ is equivalent to `Rational`, which we make equivalent to `ℚ`.
```haskell
type ℚ = Rational
```

Now, what makes something a vector space?

Well, `v` is a vector space if and only if \
there is a function `vectorAddition :: v -> v -> v` which adds any 2 elements of `v`,\
and another function `scalarMultiplication :: Rational -> v -> v` which takes a `Rational` number and "scales" an element of `v` by that number.

*There is a way to express the above notion in Haskell!*

```haskell
-- | definition of VectorSpace
class VectorSpace v where
    vectorAddition :: v -> v -> v
    scalarMultiplication :: ℚ -> v -> v
```
This reads - "For a type `v` to be a`VectorSpace`, the functions `vectorAddition :: v -> v -> v` and `scalarMultiplication :: Rational -> v -> v` need to be defined."
\ \ \ 

Now, we know that $QQ^4$ which is equivalent to `(ℚ,ℚ,ℚ,ℚ)` is a vector space. But why is that?

Well, it is so because we can define -
```
vectorAddition :: (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ)
vectorAddition (p1,p2,p3,p4) (q1,q2,q3,q4) = (p1+q1,p2+q3,p3+q3,p4+q4)
```
and we can define
```
scalarMultiplication :: ℚ -> (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ)
scalarMultiplication q (q1,q2,q3,q4) = (q*q1,q*q3,q*q3,q*q4)
```
\ \ \ 

*There is a particular way to communicate this to Haskell, which is as follows* -
```haskell
-- | example instance of VectorSpace
instance VectorSpace (ℚ,ℚ,ℚ,ℚ) where
    
    vectorAddition :: (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ)
    vectorAddition (p1,p2,p3,p4) (q1,q2,q3,q4) = (p1+q1,p2+q3,p3+q3,p4+q4)
    
    scalarMultiplication :: ℚ -> (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ)
    scalarMultiplication q (q1,q2,q3,q4) = (q*q1,q*q3,q*q3,q*q4)
```
This reads - "
\ `(ℚ,ℚ,ℚ,ℚ)` is a `VectorSpace`.
\ Why?
\ Because we give a definition for `vectorAddition :: (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ)`
\ and a definition for `scalarMultiplication :: ℚ -> (ℚ,ℚ,ℚ,ℚ) -> (ℚ,ℚ,ℚ,ℚ)`
"
\ \ \

We know that `ℚ` itself is also a `VectorSpace`. So let's tell that to Haskell -
```haskell
-- | scalar field is VectorSpace
instance VectorSpace ℚ where
    
    vectorAddition :: ℚ -> ℚ -> ℚ
    vectorAddition = (+)
    
    scalarMultiplication :: ℚ -> ℚ -> ℚ
    scalarMultiplication = (*)
```

== Other Typeclasses

Haskell allows us to define other kinds of spaces as well, not just the notion of `VectorSpace`.

Just like \ a type `v` is a `VectorSpace` if we can define `vectorAddition :: v -> v -> v` and `scalarMultiplication :: ℚ -> v -> v`,
\ a type `t` is said to be an `Eq` space if we can define a notion of equality `(==) :: t -> t -> t`.

Let's see the code for this -
```
-- | definition of Eq
class Eq t where
    (==) :: t -> t -> Bool
```
This reads - "For a type `t` be an `Eq` space, the function `(==) :: t -> t -> t` needs to be defined."

Recall the type definition
```haskell
data Colour = Red | Green | Blue
```

Now, we can make `Colour` an `Eq` space - 
```haskell
-- | Colour is an Eq space
instance Eq Colour where

    (==) :: Colour -> Colour -> Bool
    Red   == Red   = True
    Blue  == Blue  = True
    Green == Green = True
    _     == _     = False
```
This reads - "`Colour` is an `Eq` space, since we can define when two `Colour`s are equal or not by defining the function `(==) :: Colour -> Colour -> Bool`."

Haskell also inbuilt definitions for - 
```
(==) :: Integer -> Integer -> Bool
(==) :: Char -> Char -> Bool
(==) :: Bool -> Bool -> Bool
```
Thus `Integer`, `Char`, and `Bool` are `Eq` spaces.

Remember this datatype?
```
data Point = Coord {
  x_coord :: Integer,
  y_coord :: Integer
}
```
Any element of `Point` represents a point on the $RR^2$ plane.

We can tell Haskell that `Point` is also an `Eq` space -
```
instance Eq Point where

    (==) :: Point -> Point -> Bool
    point1 == point2 = x_coord point1 == x_coord point2
                    && y_coord point1 == y_coord point2
```
This reads - "`Point` is an `Eq` space, since we can define when two `Point`s are equal or not by defining that their `x_coord`inates need to be equal and their `y_coord`inates need to be equal."

== General Definition

Such a notion of a "space" (like `VectorSpace` or `Eq` space), defined by the existence of certain functions (like `vectorAddition` or `(==)` respectively) is called a typeclass.

#def(sub:"typeclass")[
  A *typeclass* is a _collection of types_, \ \ and this collection should be the collection of all types `t` such that some polymorphic functions `f1`,`f2`,`f3`, ... ,`fn` are defined for the type `t`.
]

#def(sub:"method")[
    These functions `f1`,`f2`,`f3`, ... ,`fn` are said to be *methods of the typeclass* that they help define.
]

So, to recapitulate, we have seen the @definition_of_typeclass `VectorSpace`, which is the collection of all types `v` for which the @definition_of_method#[s] `vectorAddition` and `scalarMultiplication` is defined for `v`.

And we have also seen the @definition_of_typeclass `Eq`, which is the collection of all types `t` such that the @definition_of_method `(==)` is defined for `t`.

So, we now know how to define a @definition_of_typeclass in Haskell -
```
-- | defining typeclass syntax
class MyTypeClass t where
    f1 :: <some type involving t>
    f2 :: <some type involving t>
    f3 :: <some type involving t>
    .
    .
    .
    fn :: <some type involving t>
```

And we also now know how to communicate to Haskell that \ some type `MyType` is included in this @definition_of_typeclass - 
```
-- | making a type an instance of typeclass
instance MyTypeClass MyType where

    f1 :: <type of f1 with t replaced by MyType>
    f1 =  <some definition for f1>

    f2 :: <type of f2 with t replaced by MyType>
    f2 =  <some definition for f2>
    
    f3 :: <type of f3 with t replaced by MyType>
    f3 =  <some definition for f3>
    
    .
    .
    .
    
    fn :: <type of fn with t replaced by MyType>
    fn =  <some definition for fn>
```
Basically, by definition,\ `MyType` is included in `MyTypeClass` \ only when the @definition_of_method#[s] of `MyTypeClass` are defined for `MyType`,\ so we just have to provide the appropriate definitions for those @definition_of_method#[s].

= The Point of Typeclasses

== The Point of Vector Spaces

Why do we define the concept of a vector space? 
Well, historically, it is because people noticed that we can prove things for vector spaces in general.

For example - 
\ We can prove that $QQ^4$ has a vector space basis.
\ We can prove that $QQ^5$ has a vector space basis.
\ We can prove that $QQ^6$ has a vector space basis.
\ We can prove that $QQ^7$ has a vector space basis.
\ .
\ .
\ .
\ 
\ In fact, we can prove that $QQ^NN$ (the set of all functions from $NN$ to $QQ$) has a vector space basis.

But it is "easier" to just prove *in general* that all vector spaces have a vector space basis.

This *general proof* will apply to all vector spaces. So the concept of a vector space is useful because it allows us to talk about and *prove properties of many objects at once*.

The above is one of the reasons why we define vector spaces.

== The Point of any Class

Similarly, @definition_of_typeclass#[es] in Haskell allow us to *write function definitions for many types at once*.

Let's suppose we wanted to define vector subtraction. What would that look like?
```haskell
-- | vectorSubtraction
vectorSubtraction v1 v2 = 
    v1 `vectorAddition` ( (-1) `scalarMultiplication` v2 )
-- basically 
-- v1 - v2 == v1 + ( -v2 ) == v1 + ( (-1) * v2 )
```

This definition should apply in any `VectorSpace`, right? \ Therefore we've *written a function definition that works for any type which is a `VectorSpace`*.

Recall the definition of the @code_of_elem function, used to answer whether a given object appears in a given list -
#text(size:0.7em)[#context(query([@code_of_elem].target).at(0))]

So long as `x` and `y` are from an `Eq` space, we can do `x == y` and this function will be defined.

Therefore we've *written a function definition that works for any list over any `Eq` space*.

#def(sub:"ad-hoc polymorphism")[
In summary, as long as we only fundamentally refer to the @definition_of_method#[s] of a @definition_of_typeclass,  we can 
define functions that work for any type in that @definition_of_typeclass.

This is called *ad-hoc polymorphism*
]

=== Types

As discussed, the function @code_of_vectorSubtraction should work for any type which is a `VectorSpace`. But then what is the type of the function?

It should be `vectorSubtraction :: v -> v -> v`, where `v` is any `VectorSpace`.

Haskell's way of saying the above is as follows -
```
>>> :type vectorSubtraction
vectorSubtraction :: VectorSpace v => v -> v -> v
```
It adds to `VectorSpace v =>` to say that `v` can be any `VectorSpace`.

Similarly for @code_of_elem -
```>>> :type elem
elem :: (Foldable t, Eq a) => a -> t a -> Bool
```
For now, `Foldable` just means something which is "like a list".

= Induced Instances

We know that if $V$ and $W$ are vector spaces, then the set $V times W$ can can be given a vector space structure as follows - 
$
  (v_1,w_1) + (v_2,w_2) &:= (v_1+v_2,w_1+w_2)
  \ c (v,w) &:= (c v , c w)
$
This is known as the *external direct sum* of vector spaces $V  xor W$.

We can express this notion in Haskell as follows -
```
instance (VectorSpace v, VectorSpace w) => VectorSpace (v,w) where

    vectorAddition :: (v,w) -> (v,w) -> (v,w)

    (v1,w1) `vectorAddition` (v2,w2) = 

                       ( v1 `vectorAddition` v2 , w1 `vectorAddition` w2 )

    
    scalarMultiplication :: ℚ -> (v,w) -> (v,w)

    c `scalarMultiplication` (v,w) = 

               ( c `scalarMultiplication` v , c `scalarMultiplication` w )
```
This reads - "If `v` and `w` are `VectorSpace`s, then `(v,w)` is a `VectorSpace` because we define `vectorAddition` and `scalarMultiplication` for `(v,w)` like so..."
\

Similarly, if `t` and `s` are `Eq` spaces, then `(t,s)` is also an `Eq` space - 
```
instance (Eq t, Eq s) => Eq (t,s) where
    (t1,s1) == (t2,s2) = ( t1 == t2 ) && ( s1 == s2 )
```
This reads - "If `t` and `s` are `Eq` spaces, then `(t,s)` is a `Eq` space because we define `vectorAddition` and `scalarMultiplication` for `(v,w)` like so..."
\ 
If `t` is an `Eq` space, then `[t]` is also an `Eq` space - 
```
instance Eq t => Eq [t] where
    []     == []       = True
    (t:ts) == (t':ts') = ( t == t' ) && ( ts == ts' ) 
    _      == _        = False
```
\ 
And so on...

= The `deriving` Keyword

In class and tutorial

= Superclasses and Subclasses

There is a @definition_of_typeclass denoted `Ord`, which means "`Ord`ered space". 


A type `t` is an `Ord`ered space (i.e., `t` is in the `Ord` @definition_of_typeclass) if and only if `==` and `<` are defined for `t`.

Thus we can recognize that `==` and `<` are the @definition_of_method#[s] for the `Ord` @definition_of_typeclass.

That means every `Ord`ered space has `==` defined for it.
Which means every `Ord`ered space is an `Eq` space.

Such a @definition_of_typeclass is called a "subclass".
#def(sub:"subclass")[
    If the set of @definition_of_method#[s] of a typeclass is a proper superset of the set of @definition_of_method#[s] of another typeclass, then the former typeclass is said to be a *subclass* of the latter.
]

The syntax for defining a `subclass` is a little different from usual. For example, let's define `Ord` - 
```
instance Eq o => Ord o where
    (<) :: o -> o -> Bool
```
Notice that we didn't mention `==` in the above definition, even though it's a @definition_of_method of `Ord`.

This is because it is already defined in `Eq`, and we make sure that `o` should also be in an `Eq` space by the `Eq o =>`.

= Laws

Is `Bool` a `VectorSpace`? No, of course not!

But what about this -
```
instance VectorSpace Bool where
    
    vectorAddition :: Bool -> Bool -> Bool
    vectorAddition = (/=)

    scalarMultiplication :: ℚ -> Bool -> Bool
    scalarMultiplication c bool = ( c /= 0 ) && bool
```
Haskell now believes that `Bool` is a `VectorSpace`.

So what gives?

Well, these addition and scaling functions *don't satisfy the vector space axioms*.

The vector space axioms are -
$
  forall "vectors" u,v,w &"and scalars" c,d 
  \ (v+w)+u =&= v+(w+u)
  \ c dot (d dot v) =&= (c dot d) dot v
  \ c dot (v+w) =&= c dot v + c dot w
  \ ( c + d ) dot v =&= c dot v + d dot v
  \ &.
  \ &.
  \ &.
$
etc.

But we have that 
$
  ( 1 + 1 ) dot #box(baseline:0.2em)[`True`] eq.not 1 dot #box(baseline:0.2em)[`True`] + 1 dot #box(baseline:0.2em)[`True`]
$
which *violates* the axiom $ ( c + d ) dot v =&= c dot v + d dot v $

We should ensure - that for any supposed `VectorSpace`, the @definition_of_method#[s] we define satisfy the axioms, otherwise we might end up with nonsense like `Bool` being a `VectorSpace`.

However, these kinds of problems can occur not just for `VectorSpace`, but for other @definition_of_typeclass#[es] as well -
\ For example, consider this following nonsense giving a wrong definition of `Bool` being an `Eq` space -
```
instance Eq Bool
    (==) :: Bool -> Bool -> Bool
    False == False = True
    False == True  = False
    True  == False = True
    True  == True  = True
```
Here, we have defined `True == False` but `False /= True`.
\ This violates the "axiom" of "symmetricity", which is - `if x==y then y==x`

#def(sub:"typeclass laws")[
Thus we should ensure - that for any type supposed to be an instance of a @definition_of_typeclass, the @definition_of_method#[s] we define satisfy some _"axioms"_, otherwise we might end up with nonsense.

These _"axioms"_ are called *Laws*.
]

