#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate

#let definition = def
#let example = it => [For example - \ #it]

// Chapter content goes here

= Polymorphism

Functions are our way, to interact with the elements of a type, and one can define functions in one of the two following ways:
+ Define an output for every single element.
+ Consider the general shape and behaviour of elements, and how they interact with other simple functions, then  build more complex function with that information.

Up until the chapter about lists, we saw how to define functions from a given type, to another given type, for example:

*`nand`* is a function that accepts 2 *`Bool`* values, and checks if it at least one of them is *`False`*. We will show two ways to write this function.

The first is too look at the possible inputs and define the outputs directly:
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

But with the addition of the List type from the previous chapter, we were able to add _new_ information to a type, in the following sense:

Consider the type *`[Int]`*, the elements of these types are lists of integers, the way one would interact with these would be to treat it as a collection of objects, in which each element is an integer.
- so to write a function for this type, one first needs to think about the fact that the _shape_ of an element looks like a list, and how one gets to the items of the list, and then treat the items like integers and write functions on them.
- A function for lists would thus have 2 components, at least conceptually if not explicit in the code itself, consider the following example:
  ```
  -- | squaring all elements of a list 
  squareAll :: [Int] -> [Int]
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
- *`drop`*, which takes a list and discards the first couple of elements as specified.
- *`elem`*, which checks in an element belong to a list.
- *`(==)`*, which checks if 2 elements are equal.

Up until now, we had been emphasizing on the _shape_ of elemets of a type, but these functions don't seem to care about it that much:
- The *`drop`* function just cares about the list structre of an element, and not what the internal item looks like.
- The *`elem`* function also does not care about the internal type of list items as long as some notion of equality if defined.
- The *`(==)`* works on all types where some notion of equality is defined (A counter example would be the type of functions: *`Int -> Int`*).

Now one can define such functions for every single type, like:
```
dropIntegers :: Int -> [Integer] -> [Integer]
dropIntegers = ...
dropChars :: Int -> [Char] -> [Char]
dropChars = ...
dropBools :: Int -> [Bool] -> [Bool]
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
        - Groups let us talk about sets where there is a notion of "combining" things together with more restriction.
        in all of these fields of study, say linear algebra, a theorem generally involes working with an object, whose exact details we don't assume, just that it satisfies the conditions required for it to be a vector space and seeing what can be done with just that much information.
    - And this is a powerful tool because solving a problem in the _abstract_ version solves the problem in all _concretized_ scenarios.
    
#quote(sub: "John Locke, An Essay Concerning Human Understanding (1690)")[
The acts of the mind, wherein it exerts its power over simple ideas, are chiefly these three: 1. Combining several simple ideas into one compound one, and thus all complex ideas are made. 2. The second is bringing two ideas, whether simple or complex, together, and setting them by one another so as to take a view of them at once, without uniting them into one, by which it gets all its ideas of relations. 3. The third is separating them from all other ideas that accompany them in their real existence: this is called *abstraction*, and thus all its general ideas are made. 
]

One of the ways abstraction is handled in Haskell, and a lot of other programming languages is *Polymorphism*.

#def(sub: "Polymorphism")[
A *polymorphic* function is one whose output type depends on the input type. Such a property of a function is called *polymorphism*, and the word itself is ancient greek for _many forms_.
]

A polymorphic function differs from functions we have seen in the following ways:
- It can take input from multiple differnt input types (not necessarily type, restrictions are allowed).
- Its output type can be differnt for different inputs types.

An example for such a function that we have seen in the previous section would be:
```
-- | drop
drop :: Int -> [a] -> [a]
drop _ []     = [] 
drop 0 ls     = ls
drop n (x:xs) = drop (n-1) xs
```
The polymorphism of this function is shown in the type *`drop :: Int -> [a] -> [a]`* where we have used variables //the variable `a`
(usually called a type variable) instead of explicitly mentioning a type. 
This still has a lot of structure, and is not the same as forgetting about types, since, for instance, the same variable is used in both the second argument and the output, so they need to be of the same type, dropping some elements from a list of integers also gives a list of integers, we still have all the safety and correctness guarantees that types give us.

#exercise(sub: "Datatypes of some list functions")[
A nice exercise would be to write the types of the following functions defined in the previous section: *`head`*, *`tail`*, *`(!!)`*, *`take`* and *`splitAt`*.
]

== A Taste of Type Classes
Consider the case of the integer functions

```
f :: Int -> Int
f x = x^2 + 2*x + 1

g :: Int -> Int 
g x = (x + 1)^2
```

We know that both functions, do the same thing in the mathematical sense, given any input, both of then have the same output, this is called function extensionality. But the does the following expression make sense in haskell?

```
-- | Function Extensionality
f == g
```

This definitely seems like a fair thing to ask, as we already have a definition for equality of mathematical functions, but we run into 2 issues:
- Is it really fair to say that? In computer science, the way things are computed matter, that is where the subject gets its name from. A lot of times, one will be able to distinguish distinguish between functions, by simply looking at which one works faster or slower on big inputs, and that might be something people might want to factor into what they mean by "sameness". So maybe the assumption that 2 functions being equal pointwise imply the functions are equal may not be wise.
- The second is that in general it is not possible, in this case we have a mathematical identity that lets us prove so, but given any 2 function, it might be that the only way to prove that they are equal would be to actually check on every single value, and since domains of functions can be infinite, this would simply not be possible to compute.

So we can't have the type of *`(==)`* to be `a -> a -> Bool`. In fact, if I try to write it, the haskell compiler will complain to me by saying

```
funext.hs:8:7: error: [GHC-39999]
    • No instance for ‘Eq (Int -> Int)’ arising from a use of ‘==’
      ... more error
  |
8 | h = f == g
  |   
```

To tackle this, we define the following:

// define the notion of methods
// the methods of a type T are functions which has some of its inputs :: T

#def(sub: "Typeclasses")[
_Typeclasses_ are a collection of types, characterized by the common _behaviour_. //methods
]

The previous section describes how one writes functions based on the _shape_ of the objects. And that different types can have some aspects of their _shape_ in common. @code_of_Function_Extensionality tells us that there are other properties shared by elements of different types, which we call their _behaviour_. By that we mean the functions that are defined for them.

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
```
#exercise(sub: "Checking if a list is sorted")[
Write the function `isSorted` which takes in a list as an argument, such that the elements of the list have a notion of ordering between them, and the output should be true if the list in an ascending order (equal elements are allowed to be next to each other), and false otherwise.
]

#linebreak()

= Higher Order Functions

One of the most powerful feature of functional programming languages is that it lets one pass in functions as argument to another function, and have funtions return other functions as outputs, these kinds of functions are known as:

#def(sub: "Higher Order Functions")[
A *higher order function* is a function that does at least one of the following things:
- It takes one or more functions as its arguments.
- It returns a function as an argument.
]

This is again a way of generalization and is very handy, as we will see in the rest of the chapter.

== Currying

Perhaps the first place where we have encountered higher order functions is when we defined `(+) :: Int -> Int -> Int` //input is not fn 
way back in #link(<intro-to-types>)[Chapter 3]. We have been suggesting to think of the type as `(+) :: (Int, Int) -> Int`, because that is really what we want the function to do, but in haskell it would actually mean `(+) :: Int -> (Int -> Int)`, which says the function has 1 interger argument, and it returns a function of type `Int -> Int`.

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
Category theorists call the above condition _naturality_. 
The notation $Y^X$ is the set of functions from $X$ to $Y$.])[
We prove the above by defining $"curry" : C^(A times B) -> (C^B)^A$, and then defining its inverse.
$
"curry"(f) :equiv x |-> (y |-> f(x, y))
$
The inverse of $"curry"$ is called $"uncurry": (C^B)^A -> C^(A times B)$
$
"uncurry"(g) :equiv (x, y) |-> g(x)(y)
$
To complete the proof we need to show that the above functions are inverses.
#exercise[Show that the uncurry is the inverse of curry, and that the _naturality_ condition holds.

(Note that one needs to show that uncurry is the 2-way inverse of curry, i.e, $"uncurry" compose "curry" = "id"$ and $"curry" compose "uncurry" = "id"$, one direction is not enough.)]
]

The above theorem, is a concretization of the very intuitive idea:

This may seem odd at first, but the relation between the two kinds of functions is not that hard to see, at least intuitively:
- Given a function $f$ that takes in a pair of type $(A, B) ->C$, if one fixes the first argument, then we get a function $f(A, -)$ which would take an element of type $B$ and then give an element of types $C$.
- But every different value of type $A$ that we fix, we get a differnt function.
- Thus we can think of $f$ as a function that takes in an element of type $A$ and returns a function of type $B->C$

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
suc :: Int -> Int 
suc = (+ 1) -- suc 5 = 6

-- | curry examples
neg :: Int -> Int 
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
square :: Int -> Int 
square x = x * x

-- checks if a number is the same if written in reverse
is_palindrome :: Int -> Bool 
is_palindrome x = (s == reverse s)
  where
    s = show x -- convert x to string 

is_square_palindrome :: Int -> Bool 
is_square_palindrome = is_palindrome . square
```

Breaking a complicated function into simpler parts, and being able to combime them is fair standard problem solving strategy, in both Mathematics and Computer Science, and in fact in a lot more general scenarios too! Having a clean notation for a tool that used fairly frequently is always a good idea!

Higher order functions are where polymorphism shines it brightest, see how the composition function works on all pairs of functions that can be composed in the mathematical sense, this would have been significantly less impressive if say it was only composition between functions from `Int -> Int` and `Int -> Bool`.

Another similar function that makes writing code in haskell much cleaner is the following:
```
-- | function application function
($) :: (a -> b) -> a -> b
f $ a = f a
```
This may seem like a fairly trivial function that really doesn't offer anything apart from an extra `$`, but the following 2 lines make it useful

```
-- | operator precedence
-- The 'r' in infixr says a.b.c = a.(b.c)
infixr 9 . 
infixr 0 $
```

These 2 lines are saying that, whenever there is an expression, which contains both `($)` and `(.)`, haskell will first evaluate `(.)`, using these 2 one can write a chain of function applications as follows:
```
-- old way 
f (g (h (i x)))

-- new way
f . g . h . i $ x
```
which in my opinion is much simpler to read!

#exercise[
Write a function `repeat` that takes a function `f` and an argument `a` along with a natural number `n` and applies the function `n` times on `a`, for exaxmple: `repeat (+1) 5 3` would return `8`. Also figure out the type of the function.
]

=== A Short Note on Type Inference
Haskell is a statically typed language. What that means is that it requires the types for the data that is being processed by the program, and it needs to for an analysis that happens before running the program, this is called *type checking*.

It is not however required to give types to all functions (we do strongly recommend it though!), in fact one can simply not give any types at all. This is possible because the haskell compiler is smart enough to figure all of it out on its own! It's so good that when you do write type annotations for functions, haskell ignores it, figures the types out on its own and can then check if you have given the types correctly. This is called *type inference*.

Haskell's type inference also gives the most general possible type for a function. To see that, one can open ghci, and use the `:t` command to ask haskell for types of any given expression.

```
>>> :t flip
flip :: (a -> b -> c) -> b -> a -> c
>>> :t (\ x y -> x == y) 
(\ x y -> x == y) :: Eq a => a -> a -> Bool
```

The reader should not be equipped with everything they need to understand how types can be read and can now use type inference like this to understand haskell programs better.

=== Higher Order Functions on Maybe Type

The *Maybe Type*, as defined in #link(<maybe>)[Chapter 3] is another playground for higher order functions.

When using the `Maybe` types, one eventually runs into a problem that looks something like this:
- Break up the problem into a bunch of tiny steps, so make a lot of simple function and the final solution is to be achieved by combining all of them.
- Turns out that one the functions, maybe something in the very beginning returns a `Maybe Int` instead of an `Int`.
- This means that the next function along the chain, would have had to have its input type as `Maybe Int` to account for the potentially case of `Nothing`.
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

(<$>) :: (a -> b) -> Maybe a -> Maybe b
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

#exercise(sub: "Beyond map")[
    The above shows how haskell can elegantly handle cases when we want to convert a function from type `a -> b` to a function from type `Maybe a -> Maybe b`. This can be thought of as some sort of a _change in context_, where our function is now aware that its inputs can contain a possible fail value, which is `Nothing`. The reason for needing such a _change in context_ were function of type `f :: a -> Maybe b`, that is ones which can fail. They add the possiblility of failure to the _context_.

But since we have the power to be able to change _contexts_ whenever wanted easily, we have a responsibility to keep it consistent when it makes sense. That is, what if there are multiple function with type `f :: a -> Maybe b` we then would just want to use `<.>` or `maybeMap` to get something like:
```
v :: Maybe a
f :: a -> Maybe b

g = f <$> a -> Maybe (Maybe b)
```
This is most likely going to be undesirable, the point of `Maybe` was to say that there is a possiblility of error, the point of `maybeMap` was to propogate that possible error, so when there are multiple places where the program can fail, one can define `maybeJoin :: Maybe (Maybe a) -> Maybe a`, with that we can have
```
g = maybeJoin $ f <$> a
```
This particular combination of doing `<$>` then `maybeJoin` will be very common, so people that use haskell put the 2 together in the function `(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b` (the order of operands is reversed), this makes writing code so much cleaner, for instance:
```
val :: a 
func1 :: a -> Maybe b
func2 :: b -> Maybe c
func3 :: c -> Maybe d

final :: Maybe d
final = Maybe val 
    >>= func1 
    >>= func2
    >>= func3 
```
Define `maybeJoin` and `(>>=)` and see how both of then are used in programs, and maybe compare then by how one would define `final` without these.
]

*Note* The symbol `(>>=)` is written as #unligate([`(>>=)`]).

Higher order functions, along with polymorphism help our code be really expressive, so we can write very small amounds of code that looks easy to read, which also does a lot. In the next chapter we will see a lot more examples of such functions.

<<<<<<< HEAD
=======

>>>>>>> parent of 739b177 (auto compile test)
// cite 
// citation 1
// citation 2
