#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate
#import "@preview/commute:0.3.0": node, arr, commutative-diagram

#let definition = def
#let example = it => [For example - \ #it]

// Chapter content goes here

= Polymorphism <poly>
== Classification has always been about _shape_ and _behvaiour_ anyway

Functions are our way, to interact with the elements of a type, and one can define functions in one of the two following ways:
+ Define an output for every single element.
+ Consider the general property of elements, that is, how they look like, and the functions defined on them.

And we have seen how to define functions from a given type to another given type using the above ideas, for example:

*`nand`* is a function that accepts 2 *`Bool`* values, and checks if it at least one of them is *`False`*. We will show two ways to write this function.

The first is too 
look at the possible inputs and define the outputs directly:
```
nand :: Bool -> Bool -> Bool
nand False _    = True
nand True True  = False
nand True False = True
```

The other way is to define the function in terms of other functions and how the elements of the type *`Bool`* behave
```
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)
```

The situation is something similar, for a lot of other types, like *`Int`*, *`Char`* and so on.

But with the addition of the List type from the previous chapter, we were able to add _shape_ to the elements of a type, in the following sense:

Consider the type *`[Integer]`*, the elements of these types are lists of integers, the way one would interact with these would be to treat it as a collection of objects, in which each element is an integer.
- A function for lists would thus have 2 components, at least conceptually if not explicit in the code itself:
    - The first being that of a list, which can be interacted with using functions like `head`.
    - The second being that of `Integer`, So that functions on `Integer` can be applied to the elements of the list. 
  consider the following example:
  ```
  -- | squaring all elements of a list 
  squareAll :: [Integer] -> [Integer]
  squareAll []       = []
  squareAll (x : xs) = x * x : squareAll xs  
  ```
  Here, in the definition when we match patterns, we figure out the shape of the list element, and if we can extract an integer from it, then we square it and put it back in the list.

Something similar can be done with the type *`[Bool]`*:
- Once again, to write a function, one needs to first look at the _shape_ an element as a list, Then pick elements out of them and treat them as *`Bool`* elements.
- An example of this will be the *`and`* function, that takes in a collection of *`Bool`* and returns *`True`* if and only if all of them are *`True`*.
  ```
  -- | and
  and :: [Bool] -> Bool 
  and []       = True  -- We call scenarios like this 'vacuously true'
  and (x : xs) = x && and xs
  ```
  Once again, the pattern matching handles the shape of an element as a list, and the definition handles each item of a list as a *`Bool`*.

Then we see functions like the following: 
- *`elem`*, which checks in an element belong to a list.
- *`(==)`*, which checks if 2 elements are equal.
- *`drop`*, which takes a list and discards a specfied about of items in the list from the beginning.

These functions seem to note care about all of the properties (shape and behaviour together) of their inputs.
- The *`elem`* function wants its inputs to be list does not care about the internal type of list items as long as some notion of equality if defined.
- The *`(==)`* works on all types where some notion of equality is defined, this is the only behaviour it is interested in. (A counter example would be the type of functions: *`Integer -> Integer`*, and we will discuss why this is the case soon.)
- The *`drop`* function just cares about the list structre of an element, and does not look at the behaviour of the list items at all.

To define any function in haskell, one needs to give them a type, haskell demands so, so lets look at the case of the `drop` function. One possible way to have it would be to define one for every single type, as shown below:
```
dropIntegers :: Integer -> [Integer] -> [Integer]
dropIntegers = ...
dropChars :: Integer -> [Char] -> [Char]
dropChars = ...
dropBools :: Integer -> [Bool] -> [Bool]
dropBools = ...
.
.
.
```
but that has 2 problems:
- The first is that the defintion of all of these functions is the exact same, so doing this would be a lot of manual work, and one would also need to have different name for different types, which is very inconvenient.
- The second, and arguably a more serious issue, is that it stops us from abstracting, abstraction is the process of looking at a scenario and removing information that is not relevant to the problem. 
    - An example would be that the `drop` simply lets us treat elememts as lists, while we can ignore the type of items in the list.
    - All of Mathematics and Computer Science is done like this, in some sense it is just that.
        - Linear Algebra lets us treat any set where addition and scaling is defined as one _kind_ of thing, without worrying about any other structure on the elements.
        - Metric Spaces let us talk about all sets where there is a notion of distance.
        - Differential Equations let us talk about "change" in many different scenarios.
        in all of these fields of study, say linear algebra, a theorem generally involes working with an object, whose exact details we don't assume, just that it satisfies the conditions required for it to be a vector space and seeing what can be done with just that much information.
    - And this is a powerful tool because solving a problem in the _abstract_ version solves the problem in all _concretized_ scenarios.
    
#quote(sub: "John Locke, An Essay Concerning Human Understanding (1690)")[
The acts of the mind, wherein it exerts its power over simple ideas, are chiefly these three: 
1. Combining several simple ideas into one compound one, and thus all complex ideas are made. 
2. The second is bringing two ideas, whether simple or complex, together, and setting them by one another so as to take a view of them at once, without uniting them into one, by which it gets all its ideas of relations. 
3. The third is separating them from all other ideas that accompany them in their real existence: this is called *abstraction*, and thus all its general ideas are made. 
]

One of the ways abstraction is handled in Haskell, and a lot of other programming languages is *Polymorphism*.

#def(sub: "Polymorphism")[
A *polymorphic* function is one whose output type depends on the input type. Such a property of a function is called *polymorphism*, and the word itself is ancient greek for _many forms_.
]

A polymorphic function differs from functions we have seen in the following ways:
- It can take input from multiple different input types (not necessarily all types, restrictions are allowed).
- Its output type can be different for different inputs types.

An example for such a function that we have seen in the previous section would be:
```
-- | drop
drop :: Integer -> [a] -> [a]
drop _ []     = [] 
drop 0 ls     = ls
drop n (x:xs) = drop (n-1) xs
```
The polymorphism of this function is shown in the type *`drop :: Integer -> [a] -> [a]`* where we have used the variable `a` (usually called a type variable) instead of explicitly mentioning a type. 

The goal of polymorphic functions is to let us *abstract* over a collection of types. That take a collection of types, based on some common property (either shape, or behaviour, maybe both) and treat that as a collection of elements. This lets us build functions that work on "all lists" or "all maybe types" and so on. 

The example @code_of_drop brings together all types of lists and only looks at the _shape_ of the element, that of a list, and does not look at the bhevaiour at all. This is shown by using the type variable `a` in the definition, indicating that we don't care about the properties of the list items.

#exercise(sub: "Datatypes of some list functions")[
A nice exercise would be to write the types of the following functions defined in the previous section: *`head`*, *`tail`*, *`(!!)`*, *`take`* and *`splitAt`*.
]

We have now given a type to one of the 3 functions discussed above, by giving a way to group together types by their common _shape_. This is not enough to give types of the other two functions (`(==)` and `elem`), to do so we define the following:

#def(sub: "Behaviour")[
    Given a type `T`, the *behaviour* of the elements in `T` is the set of definable functions whose type includes `T`. 
]

We use this to define the two types of polymorphism, one of which we have already seen in this section, and we will look at the other one more deepy in the next.

#def(sub: "2 Types of Polymorphism")[
- Polymorphism done by grouping types that with common _shape_ is called *Parametric Polymorphism*.
- Polymorphism done by grouping types that with common _behaviour_ is called *Ad-Hoc Polymorphism*.
]


We will come back to *parametric polymorphism* in the second half of the chapter, but for now we discuss *Ad-Hoc polymorphism*.

== A Taste of Type Classes
Consider the case of the `Integer` functions

```
f :: Integer -> Integer
f x = x^2 + 2*x + 1

g :: Integer -> Integer
g x = (x + 1)^2
```

We know that both functions, do the same thing in the mathematical sense, given any input, both of then have the same output, so mathematicans call them the same, and write $f=g$ this is called *function extensionality*. But the does the following expression make sense in haskell?

```
-- | Function Extensionality
f == g
```

This definitely seems like a fair thing to ask, as we already have a definition for equality of mathematical functions, but we run into 2 issues:
- Is it really fair to say that? In computer science, we care about the way things are computed, that is where the subject gets its name from. A lot of times, one will be able to distinguish distinguish between functions, by simply looking at which one works faster or slower on big inputs, and that might be something people would want to factor into what they mean by "sameness". So maybe the assumption that 2 functions being equal pointwise imply the functions are equal is not wise.
- The second is that in general it is not possible, in this case we have a mathematical identity that lets us prove so, but given any 2 function, it might be that the only way to prove that they are equal would be to actually check on every single value, and since domains of functions can be infinite, this would simply not be possible to compute.

So we can't have the type of *`(==)`* to be `a -> a -> Bool`. In fact, if I try to write it, the haskell compiler will complain to me by saying

```
funext.hs:8:7: error: [GHC-39999]
    • No instance for ‘Eq (Integer -> Integer)’ arising from a use of ‘==’
      ... more error
  |
8 | h = f == g
  |   
```

To tackle the problem of giving a type for `(==)`, we define the following:

// define the notion of methods
// the methods of a type T are functions which has some of its inputs :: T

#def(sub: "Typeclasses")[
_Typeclasses_ are a collection of types, characterized by the common _behaviour_. //methods
]

The previous section talked about grouping types together by the common _shape_ of the elements but @code_of_Function_Extensionality tells us that there are other properties shared by elements of different types, which we call their _behaviour_. By that we mean the functions that are defined for them.

Typeclasses are how one expresses in haskell, what a collection of types looks like, and the way to do so is by defining the common functions that work for all of them. Some examples are: 
- `Eq`, which is the collection of all types for which the function *`(==)`* is defined.
- `Ord`, which is the collection of all types for which the function *`(<)`* is defined.
- `Show`, which is the collection of all types for which there is a function that converts them to `String` using the function *`show`*.

Note that in the above cases, defining one function lets you define some other functions, like *`(/=)`* for `Eq` and *`(<=)`*, *`(>=)`* and others for the `Ord` typeclass.

Now we come back to the `elem` function, the goal of this function is to check if a given element belongs to a list. And the following is a way to write it:

```
elem _ []       = False
elem e (x : xs) = e == x || elem e xs
```
Now lets try to give this a type. 

First we see that the `e` must have the same types as the items in the list, but if we try to give it the type 

```
elem :: a -> [a] -> Bool
``` 

we will encounter the same issue as we did in @code_of_Function_Extensionality, because of `(==)`. We need to find a way to say that `a` belongs to the collection `Eq`, and this leads to the correct type:
```
elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem e (x : xs) = e == x || elem e xs
```
#exercise(sub: "Checking if a list is sorted")[
Write the function `isSorted` which takes in a list as an argument, such that the elements of the list have a notion of ordering between them, and the output should be true if the list in an ascending order (equal elements are allowed to be next to each other), and false otherwise.
]

#exercise(sub: "Shape is behaviour?")[
The two types of polymorphism, that is parametric and ad-hoc, are not exlusive, there are plenty of function where both are seen together, an example would be `elem`.

These two happen to not be that different conceptually either, we give elements their _shape_ using functions, try figuring out what the functions are for list types, maybe type, tuples and either type.

That being said, the syntax used to define parametric polymorphism sets us to set operations while defining the type of the function which is very powerful.
]
 
#linebreak()

= Higher Order Functions

One of the most powerful features of functional programming languages is that it lets one pass in functions as argument to another function, and have functions return other functions as outputs, these kinds of functions are known as:

#def(sub: "Higher Order Functions")[
A *higher order function* is a function that does at least one of the following things:
- It takes one or more functions as its arguments.
- It returns a function as an argument.
]

This is again a way of generalization and is very handy, as we will see in the rest of the chapter.

== Currying <curry>

Perhaps the first place where we have encountered higher order functions is when we defined `(+) :: Int -> Int -> Int` //input is not fn 
way back in #ref(<intro-to-types>). We have been suggesting to think of the type as `(+) :: (Int, Int) -> Int`, because that is really what we want the function to do, but in haskell it would actually mean `(+) :: Int -> (Int -> Int)`, which says the function has 1 interger argument, and it returns a function of type `Int -> Int`.

An example from mathematics would be finding the derivative of a differentiable function $f$ at a point $x$. This is generally represented as $f'(x)$ and the process of computing the derivative can be given to have the type 
$ (f, x) |-> f'(x) : ((RR -> RR)^d times RR) -> RR $
Here $(RR -> RR)^d$ is the type of real differentiable functions.

But one can also think of the derivative operator, that takes a differentiable function $f$ and produces the function $f'$, which can be given the following type:
$
d / (d x) : (RR -> RR)^d -> (RR -> RR)
$

In general, we have the following theorem:

// motivate currying by example of derivative operator

#proof(thm: [*Currying*: Given any sets $A, B, C$, there is a _bijection_ called $"curry"$ between the sets $C^(A times B)$ and the set $(C^B)^A$ such that given any function $f:C^(A times B)$ we have 
$ 
("curry" f) (a) (b) = f (a, b) 
$
Category theorists call the above condition _naturality_ (or say that the bijection is _natural_ ). 
The notation $Y^X$ is the set of functions from $X$ to $Y$.])[
We prove the above by defining $"curry" : C^(A times B) -> (C^B)^A$, and then defining its inverse.
$
"curry"(f) := x |-> (y |-> f(x, y))
$
The inverse of $"curry"$ is called $"uncurry": (C^B)^A -> C^(A times B)$
$
"uncurry"(g) := (x, y) |-> g(x)(y)
$
To complete the proof we need to show that the above functions are inverses.
#exercise[Show that the uncurry is the inverse of curry, and that the _naturality_ condition holds.

(Note that one needs to show that uncurry is the 2-way inverse of curry, that is,\ $"uncurry" compose "curry" = "id"$ and $"curry" compose "uncurry" = "id"$, one direction is not enough.)]
]

The above theorem, is a concretization of the very intuitive idea:
- Given a function $f$ that takes in a pair of type $(A, B) ->C$, if one fixes the first argument, then we get a function $f(A, -)$ which would take an element of type $B$ and then give an element of types $C$.
- But every different value of type $A$ that we fix, we get a different function.
- Thus we can think of $f$ as a function that takes in an element of type $A$ and returns a function of type $B->C$.

And the above theorem is also "implemented" in haskell using the following functions:
```
-- | curry and uncurry
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c 
uncurry g (a, b) = g a b
```

Currying lets us take a function with with argument, and lets us apply the function to each of them one at a time, rather than applying it on the entire tuple at once. One very interesting result of that is called *partial application*.

Partial applicaion is precisely the process of fixing some arugments to get a function over the remaining, let us look at some examples

```
suc :: Integer -> Integer 
suc = (+ 1) -- suc 5 = 6

-- | curry examples
neg :: Integer -> Integer 
neg = (-1 *) -- neg 5 = -5 
```
We will find many more examples in the next section.

== Functions on Functions

We have already seen examples of a couple of functions whose arguments themselves are functions. The most recent ones being @code_of_curry_and_uncurry, both of them take functions as inputs and return functions as outputs (note that our definition takes in functions and values, but we can always use partial application), these functions can be thought of as useful operations on functions.

Another very useful example, that a lot of us have seen is composition of functions, when we allow functions as inputs, composition can be treated like a function:
```
-- | composition
(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \a -> g (f a)

-- example
square :: Integer -> Integer 
square x = x * x

-- checks if a number is the same if written in reverse
is_palindrome :: Integer -> Bool 
is_palindrome x = (s == reverse s)
  where
    s = show x -- convert x to string 

is_square_palindrome :: Integer -> Bool 
is_square_palindrome = is_palindrome . square
```

Breaking a complicated function into simpler parts, and being able to combime them is fairly standard problem solving strategy, in both Mathematics and Computer Science, and in fact in a lot more general scenarios too! Having a clean notation for a tool that used fairly frequently is always a good idea!

Higher order functions are where polymorphism shines it brightest, see how the composition function works on all pairs of functions that can be composed in the mathematical sense, this would have been significantly less impressive if say it was only composition between functions from `Integer -> Integer` and `Integer -> Bool`.

Another similar function that makes writing code in haskell much cleaner is the following:
```
-- | function application function
($) :: (a -> b) -> a -> b
f $ a = f a

(&) :: a -> (a -> b) -> b
a & f = f a
```
These may seem like a fairly trivial function that really doesn't offer anything apart from an extra `$`, but the following 3 lines make them useful

```
-- | operator precedence
-- The 'r' in infixr says a.b.c.d is interpreted by haskell as a.(b.(c.d))
infixr 9 . 
infixr 0 $
infixl 1 &
```

These 2 lines are saying that, whenever there is an expression, which contains both `($)` and `(.)`, haskell will first evaluate `(.)`, using these 2 one can write a chain of function applications as follows:
```
-- old way 
f (g (h (i x)))

-- new way
f . g . h . i $ x

-- also
x & f & g & h & i
```
which in my opinion is much simpler to read!

#exercise[
Write a function `apply_n_times` that takes a function `f` and an argument `a` along with a natural number `n` and applies the function `n` times on `a`, for exaxmple: `apply_n_times (+1) 5 3` would return `8`. Also figure out the type of the function.
]

== A Short Note on Type Inference
Haskell is a statically typed language. What that means is that it requires the types for the data that is being processed by the program, and it needs to do so for an analysis that happens before running called *type checking*.

It is not however required to give types to all functions (we do strongly recommend it though!), in fact one can simply not give any types at all. This is possible because the haskell compiler is smart enough to figure all of it out on its own! It's so good that when you do write type annotations for functions, haskell ignores it, figures the types out on its own and can then check if you have given the types correctly. This is called *type inference*.

Haskell's type inference also gives the most general possible type for a function. To see that, one can open GHCi, and use the `:t` command to ask haskell for types of any given expression.

```
>>> :t flip
flip :: (a -> b -> c) -> b -> a -> c
>>> :t (\ x y -> x == y) 
(\ x y -> x == y) :: Eq a => a -> a -> Bool
```

The reader should now be equipped with everything they need to understand how types can be read and can now use type inference like this to understand haskell programs better.

== Higher Order Functions on Maybe Type : A Case Study <csm>

The *Maybe Type*, as defined in #link(<maybe>)[Chapter 3] is another playground for higher order functions.

As a refresher on *Maybe Types*, given a type `a`, one can add an _extra element_ to it by making it the type `Maybe a`. For example, given the type `Integer`, whose elements are all the integers, the type `Maybe Integer` will be the collection of integers along with an extra element, which we call `Nothing`. 

*Maybe Types* are meant to capture failure, for example, the @code_of_function_to_a_maybe_type defines the `reciprocal` function, which takes a rational number, and returns its reciprocal, except when the input is `0`, in which case it returns the _extra value_ which is `Nothing`.

To state that elements belong to a *Maybe Type* they are decorated with `Just`. For example:
- The type of `5` is `Integer`
- The type of `Just 5` is `Maybe Integer`.

To see an example of some functions that use `Maybe` in their type definitions are:
- A safe version of `head` and `tail`:
    - `safeHead :: [a] -> Maybe a` 
    - `safeTail :: [a] -> Maybe [a]`
- A safe way to index a list, that is a safe version of `(!!)`:
    - `safeIndex :: [a] -> Int -> Maybe a`
    
#exercise(sub: "Safety First")[Define the functions `safeHead`, `safeTail` and `safeIndex`.]

Something that should be noted is that so far in the book, `head`, `tail` and `(!!)` are the only functions for which we need safe versions. This is because these are the only functions that are not defined for all possible inputs and can hence give an error while the program executes (that would be like passing empty list to head, or idexing an element at a negative position). Every other function we have seen will always have a valid output, that is, it is literally impossible for functions to fail for not having a valid input if one only uses safe functions!

This may seem like a fairly trivial fact for those who are learning haskell as thier first programming language, but for those who has programmed in languages like Java, Python, C or so on, it is impossible to write a program that would lead to an error which is equivalent to the following:
- Nonetype does not have this attribute: Python
- Null Pointer Exception: Java
- Memory Access Violation or Segfault for derefencing a null pointer: C
If these erros have haunted you, you have our condolences, all of these would have been completely avoided if the langauge had some version of `Maybe`, or even some bare bones type system in case of python.

All of the safety provided by `Maybe` types has 1 potential drawback: When using `Maybe` types, one eventually runs into a problem that looks something like this:
- While solving a complicated problem, one would break it down into simpler parts, that would correspond to many tiny functions, that will come to gether to form the functions which solves the problem.
- Turns out that one the functions, maybe something in the very beginning returns a `Maybe Integer` instead of an `Integer`.
- This means that the next function along the chain, would have had to have its input type as `Maybe Integer` to account for the potentially case of `Nothing`.
- This also forces the output type to be a `Maybe` type, this makes sense, if the process fails in the beginning, one might not want to continue.
- The `Maybe` now propogates in this manner through a large section of your code, this means that a huge chunk of code needs to be rewritten to looks something like:
  ```
  f :: a -> b
  f inp = <some expression to produce output>

  f' :: Maybe a -> Maybe b
  f' (Just inp) = Just $ <some epression to produce output>
  f' Nothing    = Nothing
```
Note that `$` here is making our code a little bit cleaner, otherwise we would have to put the enter expression in paranthesis.

This is still not a very elegant way to write things though, and its just a lot of repetitive work (all of it is just book keeping really, one isn't really adding much to the program by making these changes, except for safety, programmers usually like to call it boilerplate.)

Instead of going and modifying each function manually, we make a function modifier, which is precisely what a higher order function: Our goal, which is obvious from the problem: `(a -> b) -> (Maybe a -> Maybe b)` and we define it as follows:

```
-- | maybeMap
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f (Just a) = Just . f $ a
maybeMap _ Nothing  = Nothing

(<$>) :: (a -> b) -> Maybe a -> Maybe b -- symbol version
f <$> a = maybeMap f a

(<.>) :: (b -> c) -> (a -> Maybe b) -> a -> Maybe c
g <.> f = \x -> g <$> f x

infixr 1 <$> 
infixr 8 <.>
```

*Note*: The symbol `<$>` is written as #unligate([`<$>`]).

So consider the following chain of functions:
```
f . g . h . i . j $ x
```
where say `i` was the function that turned out to be the one with `Maybe` output, the only change we need to the code would be the following!
```
f . g . h <.> i . j $ x
``` 

Higher order functions, along with polymorphism help our code be really expressive, so we can write very small amounds of code that looks easy to read, which also does a lot. In the next chapter we will see a lot more examples of such functions.

#exercise(sub: "Beyond map")[
    The above shows how haskell can elegantly handle cases when we want to convert a function from type `a -> b` to a function from type `Maybe a -> Maybe b`. This can be thought of as some sort of a _change in context_, where our function is now aware that its inputs can contain a possible fail value, which is `Nothing`. The reason for needing such a _change in context_ were function of type `f :: a -> Maybe b`, that is ones which can fail. They add the possiblility of failure to the _context_.

But since we have the power to be able to change _contexts_ whenever wanted easily, we have a responsibility to keep it consistent when it makes sense. That is, what if there are multiple function with type `f :: a -> Maybe b` we then would just want to use `<.>` or `maybeMap` to get something like:
```
f :: a -> Maybe b
g :: b -> Maybe c

h x = g <$> f x :: a -> Maybe (Maybe c)
```
This is most likely undesirable, the point of `Maybe` was to say that there is a possiblility of error, the point of `(<$>)` was to propogate that possible error then the type `Maybe (Maybe c)` seems to not have a place here.

To rectify this, we find a way to compose such functions together:
```
maybe_comp :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
infixr 8 >=>
```
This cute looking function is called the *fish* operator. This will be our way to compose functions of the shape `a -> Maybe b` together, but note that the order of inputs is reversed, so it not looks like a pipe through which the value is passed. The above function `h` is defined as follows:
```
h = f >=> g :: a -> Maybe c
```
This function, takes a value of type `a`, first applies `f` to it, and then applies `g` to it in a way that the final output is of type `Maybe c`, and of course, we can use this to make longer chains!

```
func1 :: a -> Maybe b
func2 :: b -> Maybe c
func3 :: c -> Maybe d
func4 :: d -> Maybe e

final :: a -> Maybe e
final = func1 >=> func2 
    >=> func3 >=> func4 
```
Define and `(>=>)` and see how both of then are used in programs, and compare then by how one would define `final` without these.
]

*Note* The symbol `(>=>)` is written as #unligate([`(>=>)`]).

= Exercise
#exercise(sub: "Guard Idiom")[
(i) Sometimes we have a boolean check that decides whether the return value is a failure or success. Write a function `ensure :: Bool -> a -> Maybe a` which returns `Nothing` if the boolean is `False` and `Just inp` when the boolean is `True` and `inp` is the other input.

(ii) Write a function `guard :: Bool -> Maybe ()` which gives `Nothing` when the boolean is `False` and `Just ()` when it is `True`

(iii) Write an operator `($>) :: Maybe a -> b -> Maybe b` which is a no-op on `Nothing` values, but replaces whatever is inside a `Just` value on the left with the value on the right.

(iv) Can you now write `ensure` using only `guard` and `$>`? This is called the Guard-Sequence idiom and is extremely common in production level Haskell code.

(v) While we don't use it here, could you define `(*>) :: Maybe a -> Maybe b -> Maybe b` which returns `Nothing` and returns the second argument if the first argument is `Nothing` or a `Just` value respectively. This is also used in tandem with `guard`.

#footnote[The `$>` and `*>` are part of the `Data.Functors` module and `guard` is part of the `Control.Monad` module. Their actual type signatures work for any functor, not just `Maybe`. We will see what functors are in later chapters.]
]

#exercise(sub : "Some List Functions")[
  (i) Define `filter :: (a -> Bool) -> [a] -> [a]` which given a predicate and list of elements, returns the list of elements satisfying the predicate.

  (ii) Define `map :: (a -> b) -> [a] -> [b]` which given a function and a list of elements, applies the function to each element and returns the new list.

  (iii) Define `concatMap :: (a -> [b]) -> [a] -> [b]` which maps a function over all the elements of a list and concatenate the resulting lists. Do not use `map` in your definition.

  (iv) Define `groupBy :: (a -> a -> Bool) -> [a] -> [[a]]` which groups adjacent elements according to some relation. In last chapter, we have seen `group` which is nothing but `groupBy (==)`. We could also have `groupBy (<=)` to get the consecutive increasing subsequences.
]


#exercise(sub:"Conditional Apply")[
  (i) Write a function `applyWhen :: Bool -> (a-> a) -> a -> a` which applies a function to a value if a condition is true, otherwise, it returns the value unchanged.

  (ii) Define `on :: (b -> b -> c) -> (a -> b) -> a -> a -> c` such that `on b u x y` runs the binary function `b` on the results of applying unary function `u` to two arguments `x` and `y`. This is again quite common in production level code as it avoids rewriting the same function over and over.

  (iii) Prove that
  ```
  applyWhen True = id
  applyWhen False f = id
  ```

  (iv) Prove that
  ```
  (*) `on` id = (*)
  ((*) `on` f) `on` g = (*) `on` (f . g)
  flip `on` f . flip `on` g = flip `on` (g . f)
  ```

  // (v) Write a function `cartesianSize :: [a] -> [a] -> Int` which takes two lists and gives the product of their length using `on`.
]

#exercise(sub : "Theorems for Free")[
  We will talk about some of the theorems in Wadler's iconic paper "Theorems for Free". From the type of a polymorphic function, we can derive a theorem which all such functions will follow.

  (i) Given `f :: a1 -> b1` and `g :: a2 ->  b2`, prove that `const (f a1) (g a2) = f (const a1 a2)`

  (ii) Given `r :: [a] -> [a]` and `f :: b -> c`, prove that `map f . r = r . map f` 

  (iii) Define `prodMap :: (a -> a1) -> (b -> b1) -> (a,b) -> (a1, b1)` and `coProdMap :: (a -> a1) -> (b -> b1) -> Either a b -> Either a1 b1` which apply two given functions to the elements of a tuple or an `Either`.

  (iv) Given `r :: (a,b) -> (a,b)` and `f :: a -> a1, g :: b -> b1`, prove that `r . prodMap f g = prodMap f g . r`

  (v) Given `r :: Either a b -> Either a b` and `f :: a -> a1, g :: b -> b1`, prove that `r . coProdMap f g = coProdMap f g . r`

  Can you guess the general scheme for the theorem we can get for free? Could you prove your hypothesis?
]

#exercise(sub : "Product and Co-Products")[

  (i) Define the function `product :: (a -> b) -> (a -> c) -> a -> (b, c)` which takes two functions $f : x |-> f x$ and $g: x |-> g x$ and returns a function $f times g : x |-> (f x, g x)$.

  (ii) Define a function `coProduct :: (b -> a) -> (c -> a) -> Either b c -> a` which takes two functions $f'$, $g'$ from different domains but same co-domain and combines them.

  One can make a commutative diagram for these functions as follows:
  
  #align(center, commutative-diagram(
  node((0, 0), [$A$]),
  node((0, 2), [$B$]),
  node((2, 0), [$C$]),
  node((1, 1), [$B times C$]),
  arr((0, 0), (1, 1), [`product f g`], "dashed"),
  arr((0, 0), (0, 2), [$f$]),
  arr((0, 0), (2, 0), [$g$]),
  arr((1, 1), (2, 0), [`snd`]),
  arr((1, 1), (0, 2), [`fst`]),
))

#align(center, commutative-diagram(
  node((0, 0), [$A$]),
  node((0, 2), [$B$]),
  node((2, 0), [$C$]),
  node((1, 1), [`Either B C`]),
  arr((1, 1), (0, 0), [`coProduct f' g'`], "dashed"),
  arr((0, 2), (0, 0), [$f^(-1)$]),
  arr((2, 0), (0, 0), [$g^(-1)$]),
  arr((2, 0), (1, 1), [`Right`]),
  arr((0, 2), (1, 1), [`Left`]),
))

Considering the 'co' prefix is used to define a talk about the dual of a function, could you guess what a dual means? A hint could be the fact that `Either` can be called a co-tuple as well as $f^(-1)$ can be called co-$f$.
]

#exercise(sub : "Composing Compose")[
  (i) Infer the type of `(.).(.)` manually. Can you see the use case? This is often defined in production level Haskell as `.: = (.).(.)` and is called the Blackbird Combinator (It also has another name but as it is much more explicit, we leave it upto your curiosity).

  (ii) Can you guess the type of `(.).(.).(.)`? Now by induction, what is the type of a similar expression with $n$ many `(.)`?

]







// cite 
// citation 1
// citation 2
