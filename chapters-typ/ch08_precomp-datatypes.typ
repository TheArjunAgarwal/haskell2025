#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate
#import "../Modules/Tree.typ" : tree, dots

= Datatypes (Once Again)

In #link(<sets>)[Chapter 4] we saw how Haskell datatypes correspond to sets of values. Like `Integer` is the set of all integers and `String -> Bool` is the set of all functions that take in a `String` as an argument and return a `Boolean` as their output. This was the first time we gave explicit attention to datatypes and learned the following:
#def(sub: "Types 1")[
A *Datatype*, in its simplest form, is the name of a set.
]

In #link(<poly>)[Chapter 6], where we defined polymorphic functions, the _shape_ and _behaviour_ of an element were 2 properties that we built off of. 

As a small recap, consider function `elem`, this is a function which checks if a given element belongs to a given list. The input requires to be a list of elements of a type, such that there is a notion of equality between types.

```
elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem e (x : xs) = e == x || elem e xs
```

Our requirements for the function are very clearly mentioned in the type. We are starting with a type `a` which has a notion of equality defined on it, as depicted by `Eq a`, and our arguments are an element of the type `a` and a list of elements of the type, that is, `[a]`. Here we used datatypes to specify the properties of the elements that we use. So we extend the previous definition

#def(sub: "Types 2")[
A *Datatype* is the name of a _homogenous_ collection of object, where the common properties, like the shape of elements, is depicted in the name.
]

Some examples of datatypes we have already seen are:
- `[Integer]`, which is the collection of lists of integers.
- `Maybe Char`, which is the collection of characters along with the extra element `Nothing`.
- `Integer -> String`, which is the collection of functions with their domain as the set of integers and range as the set of strings.
  
This definition suggests that datatypes can be used to _structure_ the data we want to work with. And this is actually something we have seen before!

In #link(<sets>)[Chapter 4], we saw operations on sets such as
- `(A, B)` being analogous to @definition_of_cartesian_product.
- `Either A B` being analogous to @definition_of_disjoint_union.
Here we will spend some time to see how we can define dataypes like these on our own.

Before getting to defining our own datatypes, its good to remember what the purpose of datatypes is: The point of datatype is to make thinking about programs simpler, for both the programmer and Haskell. This is done in the following ways:
- Types indicate the _shape_ of elements and can add information about the functions, for example:
    - `Either [Integer] Bool` tells us that every element of the type is either a list of integers, or a boolean value.
    - `Eq a => a -> [a] -> Maybe Integer` tells us that `a` has a notion of equality defined on it, and the output should be an integer, but the function can potentially fail (that is return `Nothing`).
- Types tell the compiler information about domain and codomain of functoins, which makes it possible for Haskell to prove that a huge class of functions is complete, that is, it returns a well defined answer on all inputs.
  
We will now see how to define our own types.

= Type Synonyms

The simpest way in which we can define our own types is by giving another name to an already existing type. This is done using the keyword `type` as follows:

```
-- | type aliases
type Point = (Integer, Integer)
type String = [Char] -- This is how Haskell defines String!

type Name = String
type Age = Integer

type Person = (Name, Age)
```

note that any type defined using the keyword `type` is simply an alias for another type and Haskell does not treat it any differently.

Nonetheless, this can be very helpful for interpreting the type for a human. For example the type `Person` which is an alias for `(String, Integer)`, when written as `(Name, Age)`, is very clearly meant to be a pair containg the name of a person, and their age.

= Finite Types <fin>

The step, which is really a big one, is that we will now define our own types, which contain the values that we create, this is done using the `data` keyword.

```
-- | finite types
data Colour = Red | Green | Blue

data Bool = True | False      -- This is how Haskell defines Bool!
data Ordering = LT | EQ | GT  -- This is also a type defined by Haskell

data Coin = Heads | Tails
```

The last example is there to emphasize that the `data` keyword really creates new types. The `Coin` type is a 2-element type, but is not the same as `Bool` and Haskell will give a type error if its used in its place. Each element of a type defined like this is called a *constructor*, which is name that will get its justification by the end of the chapter.

To define a function out of a finite type, one needs to define the output all all constructors, for example:

```
isRed :: Colour -> Bool 
isRed Red   = True 
isRed Blue  = False 
isRed Green = False
```

Defining finite types is really helpful when one wants to have a finite number of variants in a type, for example, there are a finite number of chess pieces, in languages that do not have a syntax that lets us do something like this, one would make do with strings. The benifit of these finite types is that now Haskell will make sure that the functions are only defined on intended values (unlike all possible strings), and will also give warnings if any function is not defined on all variants.

#exercise(sub: "Finite Types")[
Define the types `Month`, `Day` of the week and `DiceHead` as finite types.
]

= Product Types <prod>

These are what we get when take @definition_of_cartesian_product of other, simpler types. The purpose of product types is to define data, that has multiple smaller components. For example:
- A `Point` on a 2D grid which has 2 integer components.
- A `Profile` representing a profile on a dating app, which would contain the person's name, their age, some images and more information about them. We will be using the first example to keep things simple.
- Complex Numbers can be thought of as having 2 components, real and imaginary.

The first way to create a product, which is something we have already seen before is a tuple.
```
type Point = (  Int   ,  Int  )
          --   X-coord  Y-coord
```
And we can extract components using `fst` and `snd` functions. (Note `Point` is just a synonym for `(Int, Int)`).

Another way to do so is to use the `data` keyword again in a much more powerful way!
```
data Point = Coord Int Int -- Constructors can take inputs!
                 --  X   Y

x_coord, y_coord :: Point -> Int 
x_coord (Point x _) = x 
y_coord (Point _ y) = y
-- we use underscores to state that we don't care about the value
```
Here we need to define our own functions to extract components as `Point` is differnet from `(Int, Int)`.

The second important thing to highlight here is that constructors are functions! They are called so because they "construct" and element of the type associated with them, like `Coord` constructs elements of type `Point`, infact Haskell will even give us a type for it. Constructors for finite types can be thought of as functions that take 0 arguments (so, they just behave as values).

```
>>> :t Coord
Coord :: Int -> Int -> Point
```

Since defining a product type, and then defining functions to extract the components is a fairly common practice, haskell has another way to define product types.

```
data Point = Coord {
  x_coord :: Int,
  y_coord :: Int
}

-- This is how one can create an element!
origin :: Point
origin = Coord { x_coord = 0, y_coord = 0 }
```

These are called *Records* its a syntactic sugar, which means internally haskell treats it just like the previous way of defining product types, so `Coord 0 0` also works. But now we have the 2 functions `x_coord` and `y_coord` defined!

#exercise(sub: "Dating Profile")[
As described above, the profile of a dating app can be also thought of as a product type, one which is more complicated than a simple point in the 2d grid. Define the type `Profile` and try to see how elaborate you can make it. A fun rabbit hole do dive into would be to see how dating apps work.
]

#exercise(sub: "Complex Numbers")[
Define the dataype `Complex`, we will be looking at this again in later sections of the chapter.
]

= Parametric Types <para>

We will once again extend the use of `data` keyword using ideas form #link(<poly>)[Chapter 6].

We compared product types with tuples, we even treated `Point` as a special case `(Int, Int)` for a while. Turns out we can define our tuples, in its full generality as follows:
```
Tuple A B = Pair A B 

ex :: Tuple Int String
ex = Pair 5 "Heyy!"
```

Here `Tuple` is called a *parametric type*, and this is similar to how haskell defines its tuples, it just adds an extra syntactic sugar so we can write it as `(a,b)`.

Some other *parametric types* that we have seen before, and we will be discussing in depth in the next section are:
- `Maybe a`
- `Either a b`
- `[a]`, The list type
- The function type `a -> b`


= Sum Types
Sum types are what type theory people like to call @definition_of_disjoint_union. And already have seen everything we need to construct sum types:
- #link(<fin>)[Finite Types]
- #link(<prod>)[Viewing constructors as functions]
- #link(<para>)[Parmetric Types]

The purpose of having sum types is to have a collection of many possible _variants_ in a type. This is similar to what we did with #link(<fin>, "Finite types") but we can have an entire collection as a variant with the help of #link(<para>)[Parametric types]. Here are some examples:
```
IntOrString = I Integer | S String 

Maybe a = Just a | Nothing    -- This is how Haskell defines Maybe types!
Either a b = Left a | Right b -- This is how Haskell defines Either types!

Shape = Circle { radius :: Double } -- Record syntax works!
      | Square { side :: Double }
      | Rectangle { len :: Double, width :: Double }
-- Remember, the records are just syntactic sugar, whicha are converted to
-- Shape = Circle Double | Square Double | Rectangle Double Double
```

Just like finite types, to define a function on a sum type, one needs to define it on all variants. This is also called Pattern Matching!

```
area :: Shape -> Double 
area (Circle r)      = pi * r * r
area (Square s)      = s * s
area (Rectangle l b) = l * b
-- If this doesn't make a lot of sense, read the comment below the definition of the type.
```

#exercise(sub : "Better Dating Profile")[
Think of some interesting questions and possible answers for those questions / information bits for a dating profile and incorporate it into you `Profile` type.
]

= Inductive Types

*Inductive types* will be the final tool in the type constrution toolbox that we will be looking at. While it can be considered as a slight modification of the previous methods of building types, we will give it some special attention.

== Inductive Types (as a Mathematician)
We defined a @definition_of_set as a well-defined collection of objects. So consider the following description:
$
B = &{"Set of squares reachable by a bishop"\
&| "given that the bottom left square is in the set"}
$
Here is a quick refresher on the relevant rules of chess:
- There is an 8x8 square grid and each piece lies inside a square.
- Bishop is one of the chess pieces that can only move diagonally in a line, but it is allowed to move as far as possible.

One can now create the set $B$ by starting at the bottom left square and one by one adding sqaures, where the bishop can reach, to set. Here we say that the set $B$ was generated by the $"bottom left"$ under the $"bishop movement"$ rules. Here, there was one other important piece of information other than the "base case" and the "operation", the extra structure imposed by the chess board itself, specifically, the operations $angle.l"go top right" angle.r -> angle.l"go top left"angle.r$ is the same as $angle.l"go top left"angle.r -> angle.l"go top right"angle.r$ and the board is restricted to an $8 times 8$ grid.

If that was not the case then we would call the set *freely generated*. In a freely genereated set, a sequence of operations uniquely maps to an element of the set. In such cases we give the description of the set by simply giving the element and the operations. Consider the following description of natural numbers as an example:

=== Natural Numbers as Inductive Types

Let $NN$ be the set freely generated by the following:
- The element $0 :: NN$
- The operation $"succ" :: NN -> NN$

Here the names are suggestive, but if simply follow the rules for freely generated sets, we get the following:
- We know that $0$ is in the set.
- That means $"succ" 0$ is in the set.
- Which means $"succ" ("succ" 0)$ is in the set.
- and so on...

So we will end up with the set 
$ 
{space 0,space "succ" 0,space "succ succ" 0,space "succ succ succ" 0,space "succ succ succ succ" 0 space... space} 
$
Now we can 

This way is very similar to how mathematicians usually formalize natural numbers.#footnote[The usualy way to define natural numbers was written by Guiseppe Peano and are called the 'Peano Axioms' which invole a bunch of rules, whose relevant part can be summarizes as :
- $0$ is a natural number 
- Natural numbers are closed under the $"succ"$ operation 
- For each number $x$ that is not $0$, there is a unique number $y$ such that $x= "succ" y$
- $0$ is not the successor of any number
The first 2 rule are given in the definition, the next 2 are subsumed in the definition of "free generation".].

These "freely generated sets" are what programmers call Inductive types, and one can define the type of natural numbers in haskell as follows:
```
-- | nat
data Nat = Z | Succ Nat 

three :: Nat
three = Succ (Succ (Succ Z))
```

And functions on a natural number would be usually given by a recursive function:

```
add' :: Nat -> Nat -> Nat 
add'  Z       n = n
add' (Succ m) n = Succ (add' m n)
```

We can also define functions to convert between Integers and Natural Numbers:
```
-- | nat and integer 
natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger (Succ n) = natToInteger n + 1

integerToNat :: Integer -> Maybe Nat
integertoNat n | n <  0 = Nothing
               | n == 0 = Z
               | n > 1  = Succ (integerToNat (n-1)) 
```
#exercise[
Natural numbers are the default way people count things. A lot of the haskell functions that involve counting, like `(!!)`, `takeWhile`, `drop` and so on are functions that can potentially fail because of an attept to access a negative index. redefine these functions our definition of natural numbers (@code_of_nat).
]

#exercise(sub: "Functions on naturals")[
Define versions of functions `max`, `sum`, `prod`(product), `min` and `==` for natural numbers. Note that you would have to use new names.
]
=== Lists as Inductive Types

Another interesting example of an inductive type is the set of list of elements of type `A`.

Given a set/type `A` we can define the set of list elements using the following:
- $square :: [A]$, the empty list.
- For each element $a::A$, a function $(a:) :: [A] -> [A]$.

We leave it to the reader to show that this inductive type genrates the set of all lists of elementns of type `A`. We will justify for the example $[0,1,2,3]$
- $square$ belongs to the set $ZZ$
- Then $3:square$ belongs to the set $ZZ$
- Then $2:3:square$ belongs to the set $ZZ$
- Then $1:2:3:square$ belongs to the set $ZZ$
- Then $0:1:2:3:square$ belongs to the set $ZZ$
And we treat $0:1:2:3:square$ as $[0,1,2,3]$.

In haskell, we cannot write infinitely many constructors in the definition of a type, so we instead define it as follows:

```
-- | list
data List A = Nil | Cons A (List A)
```
Haskell will not let us use `[]`, this is sytactic sugar given by the compiler a user (like us) cannot give our own definitions to, so we will us `Nil` and `Cons` instead.

Fixing as as `Integer` for now `Nil` represents `[]` and `Cons` takes an integer `n` and gives the constructor `n:`. This is our workaround for having multiple constructors. The above definition (apart from the syntactic sugar) is how Haskell internally defines lists.

This idea is very much inspired by the concept of #link(<curry>)[Currying] which was discussed in chapter 6.


== Inductive Types (as a Programmer)

As a programmer, inductive types are used to indicate that the elements of a type are created from other smaller elements of the type. This is pretty much the same as what the mathematician thinks, as it should be, but a programmer puts more emphasis on the fact that the type is the _blueprint_ of the elements in it, that is, the _shape_ of the elements is reflected in the type. 

To understand this better, consider the example where you want to write a calculator. A calculator takes a simple arithmetic expression and evaulates it.

=== Calculator

For our purposes, we say that our calculator can compute:
- Addition
- Subtraction
- Multiplication
- Division
- Exponentiation

The plan will to have an inductive type `Expr` of expressions (because tiny expressions combine to give big expressions), which we define as follows:

```
-- | expression
data Expr = Val Double
          | Add Expr Expr 
          | Sub Expr Expr 
          | Mul Expr Expr 
          | Div Expr Expr 
          | Exp Expr Expr 
          | Neg Expr 
```

Here the goal of the type was to specify the structure of the data (arithmetic expression) we want to working with, lets see a few examples!

The expression $3 + 5 * 10 + 8^3/ 2$ corresponds to
```
-- | expr example
ex :: Expr 
ex = Add (Val 3.0)
         (Add (Mul (Val 5.0)
                   (Val 10.0))
              (Div (Exp (Val 8.0)
                        (Val 3.0))
                   (Val 2.0))
```

#exercise(sub:"Evaluate and extend")[
Write a function `eval::Expr -> Double` that takes an expression and returns its value. The potential failure case here is division by 0. To deal with it, either add an failure value to the expression type, or make the function have a `Maybe` output.

Also try extending the Expression type to include more operations.
]

=== Trees as Inductive Types

For those with keen eyes and good memory the shape of `ex` should remind you of the discussion in #link(<why>)[Why Trees?] section in chapter 1.

On the topic of trees, while working with such inductive one finds that all inductive dataypes follow a tree structure, this is a result of _free generation_. Trees happen to be a ubiquitous data-structure (way to structure data) in computer science and has applications everwhere. The following is a very tiny subset of those:
- Compilers (like both haskell and our calculator)
- File Systems
- Databases
- Data representation formats like JSON and XML
- Data Compression (huffman encoding)
- Space partitioning (oct-trees and quad-trees)

So we now define trees, recall @definition_of_tree, which defines a tree as a meaningful structure on data involving a main *root* node, and each node having 0 or more children, as shown in the following diagram.

$
#tree(($36$,($71$,$44$,$13$),$42$,($34$,$7$)))
$

Looking at the structure, we can define a tree as follows in haskell:
```
-- | tree
data Tree a = Node { value :: a, children :: [Tree a] }
```

And the above tree can be represented as follows:
```
ex :: Tree Integer 
ex = Node 36
        [ Node 71 [Node 44 [], Node 13 []]
        , Node 42 []
        , Node 37 [Node 7 []]]
```
#exercise(sub: "Tree Functions")[
Define the following functions for the tree datatype:
- `depth :: Tree a -> Nat`, this defines the longest path one can take starting from the route, for example, the `depth ex` is 2.
- `size :: Tree a -> Nat`, this defines the number of nodes in a tree, for example `size ex` is 7.
Also define versions of the `elem` and `sum` functions for the `Tree` datatype.
]


// cite
// Haskell Mooc
// https://jameshaydon.github.io/passport/
// citation 2
== Excercise
// Reverse Polish Notation Stack calculator...
