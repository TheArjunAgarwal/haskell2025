#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate
#import "../Modules/Tree.typ" : tree, dots

= Datatypes (Once Again) <data>

In #link(<sets>)[Chapter 4] we saw how Haskell datatypes correspond to sets of values. Like `Integer` is the set of all integers and `String -> Bool` is the set of all functions that take in a `String` as an argument and return a `Boolean` as their output. This was the first time we gave explicit attention to datatypes and learned the following:
#def(sub: "Types 1")[
A *Datatype*, in its simplest form, is the name of a set.
]

In #ref(<poly>), where we defined polymorphic functions, the _shape_ and _behaviour_ of an element were 2 properties that we built off of. 

As a small recap, consider function `elem`, this is a function which checks if a given element belongs to a given list. The input requires to be a list of elements of a type, such that there is a notion of equality between types.

```
elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem e (x : xs) = e == x || elem e xs
```

Our requirements for the function are very clearly mentioned in the type. We are starting with a type `a` which has a notion of equality defined on it, as depicted by `Eq a`, and our arguments are an element of the type `a` and a list of elements of the type, that is, `[a]`. Here we used datatypes to specify the properties of the elements that we use. So we extend the previous definition.

#def(sub: "Types 2")[
A *Datatype* is the name of a _homogenous_ collection of object, where the common properties, like the shape of elements, is depicted in the name.
]

Some examples of datatypes we have already seen are:
- `[Integer]`, which is the collection of lists of integers.
- `Maybe Char`, which is the collection of characters along with the extra element `Nothing`.
- `Integer -> String`, which is the collection of functions with their domain as the set of integers and range as the set of strings.
  
This definition suggests that datatypes can be used to _structure_ the data we want to work with. And this is actually something we have seen before!

In #ref(<sets>), we saw operations on sets such as
- `(A, B)` being analogous to @definition_of_cartesian_product.
- `Either A B` being analogous to @definition_of_disjoint_union.
Here we will spend some time to see how we can define dataypes like these on our own.

Before getting to defining our own datatypes, its good to remember what the purpose of datatypes is: The point of datatype is to make thinking about programs simpler, for both the programmer and Haskell. This is done in the following ways:
- Types indicate the _shape_ of elements and can add information about the functions, for example:
    - `Either [Integer] Bool` tells us that every element of the type is either a list of integers, or a boolean value.
    - `Eq a => a -> [a] -> Maybe Integer` tells us that `a` has a notion of equality defined on it, and the output should be an integer, but the function can potentially fail (that is return `Nothing`).
- Types tell the compiler information about domains and codomains of functions which lets it, to a great extent, check if functions are given inputs they are defined on, and that functions are defined on the values of the domain. (It is not always capable of doing so, for example trying to `sum` an infinite list, but this avoids runtime errors by a lot!)
  
We will now see how to define our own types.

= Finite Types <fin>

The first step we take in defining types is by creating values and put them together in a collection, this is done using the `data` keyword.

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

The first way to create a product, which is something we have already seen before is a tuple. As our example we will consider the type `(Integer, Integer)` which we are supposed to interpret as the the set of points on a 2-dimensional lattice. where the two `Integer`s represent the x and y coordinates of a point on the lattice.

And we can extract components using `fst` and `snd` functions. (Note `Point` is just a synonym for `(Integer, Integer)`).

Another way to do so is to use the `data` keyword again in a much more powerful way!
```
data Point = Coord Integer Integer -- Constructors can take inputs!
                 --  X        Y

x_coord, y_coord :: Point -> Integer 
x_coord (Point x _) = x 
y_coord (Point _ y) = y
-- we use underscores to state that we don't care about the value
```
Here we need to define our own functions to extract components as `Point` is differnet from `(Integer, Integer)`.

The second important thing to highlight here is that constructors are functions! They are called so because they "construct" and element of the type associated with them, like `Coord` constructs elements of type `Point`, infact Haskell will even give us a type for it. Constructors for finite types can be thought of as functions that take 0 arguments (so, they just behave as values).

```
>>> :t Coord
Coord :: Integer -> Integer -> Point
```

Since defining a product type, and then defining functions to extract the components is a fairly common practice, haskell has another way to define product types.

```
data Point = Coord {
  x_coord :: Integer,
  y_coord :: Integer
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

We will once again extend the use of `data` keyword using ideas form #ref(<poly>).

We compared product types with tuples, we even treated `Point` as a special case `(Integer, Integer)` for a while. Turns out we can define our tuples, in its full generality as follows:
```
Tuple A B = Pair A B 

ex :: Tuple Integer String
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
-- Remember, the records are just syntactic sugar, which are converted to
-- Shape = Circle Double | Square Double | Rectangle Double Double
```

Just like finite types, to define a function on a sum type, one needs to define it on all variants. This is also called Pattern Matching!

```
area :: Shape -> Double 
area (Circle r)      = pi * r * r
area (Square s)      = s * s
area (Rectangle l b) = l * b
-- If this doesn't make a lot of sense, read the comment below the definition of the type Shape.
```

#exercise(sub : "Better Dating Profile")[
Think of some interesting questions and possible answers for those questions / information bits for a dating profile and incorporate it into you `Profile` type.
]

= Inductive Types

*Inductive types* will be the final tool in the type construction toolbox that we will be looking at. While it can be considered as a slight modification of the previous methods of building types, we will give it some special attention.

== Inductive Types (as a Mathematician)
We defined a @definition_of_set as a well-defined collection of objects. So consider the following description:
$
B = &{"Set of squares reachable by a bishop (in 0 or more moves)"\
& "given that the bottom left square is in the set"}
$
Here is a quick refresher on the relevant rules of chess:
- There is an 8x8 square grid and each piece lies inside a square.
- Bishop is one of the chess pieces that can only move diagonally in a line, but it is allowed to move as far as possible.

One can now create the set $B$ by starting at the bottom left square and one by one adding sqaures, where the bishop can reach, to set. Here we say that the set $B$ was generated by the $"bottom left"$ under the $"bishop movement"$ rules. There was one other important piece of information other than the "base case" and the "operation", the extra structure imposed by the chess board itself, specifically, the operations $angle.l"go top right" angle.r -> angle.l"go top left"angle.r$ is the same as $angle.l"go top left"angle.r -> angle.l"go top right"angle.r$ and the board is restricted to an $8 times 8$ grid. This is called *generating* a set from a value (bottom left square) using a function (bishop movement).

Now consider the following example: You are trying to braid your (or your long haired friend's hair), you do so by starting with 3 bunches of hair (ordered as left, middle and right) and you plan to put them together. This is done by swapping positions of the middle bunch, with either the left or the right bunch (middle one always goes from below), done in an alternating manner. Turns our you're new to this and did not know about the alternating part. And so you start braiding. An important observation you make as you do this every other day is that every different sequence of swaps gives you a different looking braids! (most of them will probably not look good, but you are (or your friend is) a math person, this makes you happy). If we consider $B$ to be the set of all braids that you can create, we can define it using just 2 pieces of information:
- You started with the non-braid which is in the set.
- You either swapped the middle bunch with the left, or with the right bunch and kept doing this.
This is another example of a *generated* set, but here there are no extra rules other than the starting element and the operations (unlike the restrictions imposed by the geometry of the chessboard). In such cases we say that the set is *freely generated* (free as is no restrictions). A very useful fact about such sets is that each element can be identified with the sequence of operations used to create them, in fact a lot of the times this is how people talk about elements of freely generated sets.

#def(sub: "Freely Generated Sets")[
Given a collection of of *base values* $cal(B)= {b_1,b_2... b_n}$ and a collection of *operationd* $cal(F)={f_1,f_2, ... f_m}$ we say that a set $S$ is freely generated from $cal(B)$ using $cal(F)$ if $S$ satisfies the following properties:
- $cal(B) subset.eq S$, that is, all the base values are in the set.
- Given $s_1,s_2...s_n in S$ and any $f in cal(F)$ we have that $f(s_1,s_2... s_n)in S$.
- If there are 2 elements in $S$ constructed as $s_1 = f_1(v_1,v_2...v_p)$ and $s_2 = f_2(w_1,w_2...w_q)$ such that $s_1 = s_2$, then we can say that
    - $f_1 = f_2$,
    - $p = q$ and
    - $v_1 = w_1$, $v_2= w_2$... $v_p = w_q$
- $S$ is the smallest such set satisfying these properties.
]

Now we see some more examples.

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

This way is very similar to how mathematicians usually formalize natural numbers.#footnote[The usualy way to define natural numbers was written by Guiseppe Peano and are called the 'Peano Axioms' which invole a bunch of rules, whose relevant part can be summarizes as :
- $0$ is a natural number 
- Natural numbers are closed under the $"succ"$ operation 
- For each number $x$ that is not $0$, there is a unique number $y$ such that $x= "succ" y$
- $0$ is not the successor of any number.
].

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
               | n == 0 = Just Z
               | n > 1  = Just $ Succ (integerToNat (n-1)) 
```
#exercise[
Natural numbers are the default way people count things. A lot of the haskell functions that involve counting, like `(!!)`, `takeWhile`, `drop` and so on are functions that can potentially fail because of an attept to access a negative index. redefine these functions our definition of natural numbers (@code_of_nat).
]

#exercise(sub: "Functions on naturals")[
Define versions of functions `max`, `sum`, `prod`(product), `min` and `==` for natural numbers. Note that you would have to use new names.
]
=== Lists as Inductive Types <listi>

Another interesting example of an inductive type is the set of lists over the type `A`.

Given a set/type `A` we can define the set of list over it using the following:
- $square :: [A]$, the empty list.
- For each element $a::A$, a function $(a:) :: [A] -> [A]$.

We leave it to the reader to show that this inductive type generates the set of all lists of elements of type `A`. We will justify for the example $[0,1,2,3]$
- $square$ belongs to the set $[ZZ]$
- Then $3:square$ belongs to the set $[ZZ]$
- Then $2:3:square$ belongs to the set $[ZZ]$
- Then $1:2:3:square$ belongs to the set $[ZZ]$
- Then $0:1:2:3:square$ belongs to the set $[ZZ]$
And we treat $0:1:2:3:square$ as $[0,1,2,3]$.

In haskell, we cannot write infinitely many constructors in the definition of a type, so we instead define it as follows:

```
-- | list
data List A = Nil | Cons A (List A)
```
Haskell will not let us use `[]`, this is sytactic sugar given by the compiler a user (like us) cannot give our own definitions to, so we will us `Nil` and `Cons` instead (similar to the bracket and comma syntax for tuples).

Fixing as `A` as `Integer` for now `Nil` represents `[]` and `Cons` takes an integer `n` and gives the constructor `n:`. This is work around to not being able to write infintely many constructors. The above definition (apart from the syntactic sugar) is how Haskell internally defines lists.

This idea is very much inspired by the concept of #link(<curry>)[Currying] which was discussed in #ref(<curry>).


== (Not Quite) Inductive Types (as a Programmer) <nqit>

As a programmer, we will be using these types as a _blueprint_ for the shape of the element in the type. Specifically to indicate that an element is created by combining other elements of the type together, hence, these are also called *recrusive datatypes*.

In fact, the constructors defined are often used to describe a procedure to check if an element belong to the type, very similar to what we did in @definition_of_tree and in @definition_of_well-formed_mathematical_expression.

We will see how to extract such a procedure from the constructors of a type in #ref(<treei>).

But first we see a simple example of a recursive datatype, a type to represent arithmetic expressions.

=== Calculator <calc>

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

The following is the procedure to check 

#exercise(sub:"Evaluate and extend")[
Write a function `eval::Expr -> Double` that takes an expression and returns its value. The potential failure case here is division by 0. To deal with it, either add an failure value to the expression type, or make the function have a `Maybe` output.

Also try extending the Expression type to include more operations.
]

For those with keen eyes and good memory the shape of `ex` should remind you of the discussion in #link(<why>)[Why Trees?] section in #ref(<why>).

We will now justify the satement made there:
#quote(sub: "Ryan Hota, Haskell2025")[
*In fact, any object in Haskell is internally modelled as a tree-like structure.*
]

We will now see that Haskell verifies that the element `ex` (from @code_of_expr_example) is an `Expr` using a procedure that is very similar to what was defined in @definition_of_tree.

- We see that the value `ex` is created using the constructor `Add`, so it must produce an `Expr`, now we need to make sure that both  of its arguments are also of type `Expr`.
    - The fisrt argument of the above is `Val 3.0` which defines a `Expr`.
    - The second argument of the above constructor is also `Add` so it must produce an `Expr` given that both its arguments are also of type `Expr`.
        - The first argument to this is produced using the constructor `Mul` hence it must produce an element of type `Expr` given the correct arguments.
            - Its first argument is `Val 5.0`
            - Its second argument is `Val 10.0`, both of which are `Expr`.
        - The second argument to the `Add` is constructed using `Div`, so it must be a tree given both its arguments are `Expr`.
            - Its first argument is constructed using `Exp`, so it must be a `Expr` given its arguments are also `Expr`
                - Its first argument is `Val 8.0`
                - Its second argument is `Val 3.0`, both of which are `Expr`.
            - Its second argument is `Val 2.0` which is a `Expr`.

by simply checking that all constructors get inputs of the correct type, Haskell has gone through the procedure defined in @definition_of_tree to check that the element `ex` is well defined.

=== Trees as Inductive Types <treei>

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

        
=== Binary Trees
Binary Trees are a special case of trees, where each node has either exactly 2, or 0 children. Nodes with 0 children are called *leaves*.

Out of the uses cases mentioned for trees the following involve binary trees:
- Compilers (for functional languages)
- Databases
- Data compression (Huffman Encoding)

A binary tree over intergers looks like:
$
#tree(($3$,($7$,$bullet$,$bullet$),($2$,($8$, $bullet$, $bullet$), $bullet$)))
$

Here unlike the (not necessarily binary) tree, we don't allow leaves to hold values (represeted by $bullet$), these allow us to have nodes that behave like they have just 1 child (like $2$ in the above example). This also allows are definition to look like the definition of lists:

```
-- | Btree 
data Btree a = BNode {left :: Btree a, Val :: a, right :: Btree a}
             | Leaf
```

and the example above can be written as:
```
-- | Btree ex
bex:: Btree Integer 
bex = BNode (BNode 7 Leaf Leaf) 
            3
            (BNode (BNode 8 Leaf Leaf) 
                   2 
                   Leaf)
```

#exercise(sub: "Binary Tree Functions")[
Define all of the @exercise_of_Tree_Functions for binary trees.
]

We will see how these datatypes are used in #ref(<ads>).

#exercise(sub: "All trees are Binary Trees")[
There is a way to convert a (not necessarily binary) tree into a binary tree without losing any information (that is, in a way that one can reconstruct the original tree back from the binary tree).

The idea here is that a binary tree has 2 types of relations between elements, that of a left child, and that of a right child. And we use those to capture the 2 types of relations in trees which are: Being the _first_ child, being the _next_ sibling. In fact, one can get to each element of a tree from the root by these 2 operations.

So given a tree, we construct its corresponding binary tree as follows:
+ We set the root as also the root of the binary tree. 
+ If the node we are looking at in the tree is a leaf and does not have siblings to the right of it, we are done with the node.
+ If the node we are looking at is not a left, let `(x:xs)` be the list of children. Then we make an edge from the current node to `x` and we connect `x` to the next element in the list. That to the one after it and so on.
+ We repeat the previous 2 steps with all new vertices added in step 3 until there are no new nodes left to add.

As an example:
$
#tree(($36$,($71$,$44$,$13$),$42$,($34$,$7$))) quad ==> quad 
#tree(($36$, ($71$, ($44$, $bullet$, ($13$, $bullet$, $bullet$)), ($42$,$bullet$, ($34$, ($7$, $bullet$, $bullet$), $bullet$))), $bullet$))
$

Write a function `treeToBtree::Tree a -> Btree a` to convert to a tree. Also write the function `btreeToTree::Tree a -> Maybe (Btree a)` and show why `Maybe` is required here.
]

=== Addressing the "Not Quite"...
The title of @nqit is *(Not Quite) Inductive Types (as a Programmer)*. Turns out that while we have been discussing inductive types this entire section of the chapter, because of the way haskell works, all of the types that we have defined like :
- `Nat`
- `Expr`
- `Tree`
- `Btree`
- `List`
aren't exactly inductive types, a consequence of this is that the type `Nat` isn't exactly the set $NN$ either.

The culprit here is laziness and its exactly what was discussed in @dark_magic, that is infinite elements, to understand that one of the important points in @definition_of_Freely_Generated_Sets is that that the set that is being generated must be the *smallest* set that satisfies the other properties given in the definition.

Consider the case of the type `Nat = Zero | Succ Nat`, this is supposed to represent the set $NN$ and it satisfies all the properties fo a freely generated set, aka an inductive type except for one: The one about $NN$ being the smallests set that satisfies the given properties because of the following extra elements:
```
infinity :: Nat
infinity = Succ infinity

-- so the element looks like
-- infinity = Succ( Succ( Succ( Succ( Succ( Succ...)))))
``` 
So Haskell is letting the `infinity` sneak into our type `Nat`. 

The same also holds for all of the other types where we can define
```
inflist :: a -> [a]
inflist a = a : inflist a 
-- which is simply
-- a : a : a : a : a ...
```

And even infinite expressions like 
```
infexpr :: Double -> Expr 
infexpr x = Add x (infexpr x)
```
which is a funny term that is simply evalutates to 
```
infexpr x = x + x + x + x + ...
```
One can make weirder terms that make even less sense than the above and are encouraged to do so, its fun.

There is a formal way to reason about such infinite data structures, and this feature is captured in programming languages like Lean and Agda and is called *coinduction*, but we will not be discussing it. 

We still chose to put emphasis on *inductive datatypes* as we think that its an idea helpful in designing programs. A lot of the functions that we have discussed so far, like 
- `eval` from @exercise_of_Evaluate_and_extend
- `depth` and `size` from @exercise_of_Tree_Functions
- `natToInteger` from @code_of_nat_and_integer

and many more from the list chapter and so on are designed with the idea of inductive types. The purpose of types, as discussed, was to be able to give information to the Haskell compiler about what the functions should expect as an argument, this is one of the places where the Haskell type system fails to express that.

Nonetheless, most of the time people do tend to asssume that the functions are going to get arugments that are finite, which is reasonable in most cases, for example if you are writing a full fledged calculator, using the type described in @calc, it would involve something like
- Being able to take a string input
- Parse it into an `Expr` element
- Evaluate it and return the answer 

In such cases it is very easy to make sure that expressions are going to be finite (I don't any user has enough free time to enter an infinite expression).

If you are disappointed by the fact that the Haskell compiler is letting a potential error pass through, not that the alternative would be, being able to prove that all programs in haskell would terminate. This problem is a version of the *Halting Problem* which is one of the most famous problems in computer science and, in some sense, is the problem that the field of modern computer science stems from. Alan Turing proposed this problem and prove that it is _unsolvable_, so it simply isn't possible for a language like haskell to check for infinite structures like this. Languages like Lean and Adga achieve this by also disallowing some programs that do terminate. These languages are not Turing Complete (its not possible to write every valid program in this language).

= Same Same but Different
This section is a bit differnt from the previous one, the goal here is not to create new types but to repurpose the old ones.

== Same Same
One of the two reasons we had given in #ref(<data>) for creating types was to make programs make more sense to people who try to read them. Datatypes often indiciate the structure of the code and it doing so, they can express the intent of the programmer. One of the ways in which haskell makes this easy for us by letting us come up with aliases for types. This is done using the `type` keyword.

```
-- | type aliases
type Point = (Integer, Integer)
type String = [Char] -- This is how Haskell defines String!

type Name = String
type Age = Integer

type Person = (Name, Age)
```
note that any type defined using the keyword `type` is simply an alias for another type and Haskell does not treat it any differently.

Nonetheless, this can be very helpful for interpreting the type for a human. For example the type `Person` which is an alias for `(String, Integer)`, when written as `(Name, Age)`, is very clearly meant to be a pair containg the _name_ and _age_ of a *person*.

== Different

Another thing one might want to do with an existing type is to use it as your own and define your own functions on it. 

One reason you might want to do this would be to use the same datatype to for different pieces of data, and you might not want them to get mixed up. For example, say you're playing a game involving you going down a cave to find treasures. Your 'health' would be an `Integer`, if that goes to `0` your character dies. The 'depth' you're at would also be an `Integer` which would be used to decide how valuable the minerals you find would be. (For those interested, the game in mind is Minecraft).

We can use `data` to let us define separate types like:
```
data Health = Health Integer 
data Depth = Depth Integer
```

Now you have 2 copies of `Integer` that haskell will treat differently, making sure that they never get mixed up. 

One change that we can make here is to use the `newtype` keyword instead of `data` and get the following:

```
newtype Health = Health Integer 
newtype Depth = Depth Integer
```

This just tells Haskell that your datatype has only 1 constructor with exacty 1 field which will allow Haskell to apply some optimizations. It otherwise behaves just like `data` except for the above mentioned restrictions.

= Exercise
#exercise(sub : "JSON")[
The JSON (JavaScript Object Notation) data format consists of strings, numbers, booleans, null, arrays (lists), and objects (key-value maps).

(a) Define a recursive Haskell datatype `JsonValue` that can represent any valid JSON structure.

(b) Using your new datatype, represent the following JSON object in Haskell:
```
{
  "name": "Alan Turing",
  "born": 1912,
  "is_alive": false,
  "contributions": ["Turing Machine", "Turing Test"]
}
```
]
#exercise(sub : "File System")[
(a) Design a datatype `FileSystemItem` to represent a hierarchical file system. A file system consists of two types of items:
- A `File`, which has a name (String) and content (String).
- A `Directory` or Folder, which has a name (String) and contains a list of other `FileSystemItems`.

(b) Implement a function `find :: String -> FileSystemItem -> Maybe String` that searches for a file by name within a directory (and all its subdirectories) and returns its content if found.
]

#exercise(sub : "Folding a Tree")[
    (a) For the `Tree a` defined in this chapter, implement `foldTree :: (a -> [b] -> b) -> Tree a -> b` which takes a node's value (`a`) and the list of folded results from its children (`[b]`) and produces a new result (`b`). 
    
    (b) Use `foldTree` to implement `size`, `depth`, and `sum` for the `Tree` type.
]

#exercise(sub : "Git")[
    At its core, Git is just a content-addressed filesystem built on a few simple object types.
        - Blob: Represents the content of a file. It's just a chunk of data.
        - Tree: Represents a directory. It contains a list of pointers to Blobs (for files) and other Trees (for subdirectories). Each pointer includes the item's name, type (blob/tree), and its hash.
        - Commit: Represents a snapshot in time. It contains a pointer to a single top-level Tree, pointer(s) to parent Commit(s), an author, a committer, and a message.

    Model these three object types in Haskell. Use `String` to represent SHA-1 hashes (the "pointers"). Then, define a function `findFileInCommit :: Commit -> FilePath -> Maybe Blob` that, given a commit and a path like "src/main/core.hs", traverses the tree structure to find the corresponding blob.
]

#exercise(sub : "Matrix")[
    Define `Matrix` as a type alias for `[[a]]` where `a` is number. We want you to implement

    (i) `check :: Matrix a -> Bool` to check if the matrix is valid (all rows have the same number of elements, all columns have the same number of elements).

    (ii) `transpose :: Matrix a -> Matrix a` to take the transpose of the matrix

    (iii) `addMat :: Matrix a -> Matrix a -> Matrix a` and `subMat :: Matrix a -> Matrix a -> Matrix a` to add and subtract matrices.

    (iv) `det :: Matrix a -> Double` to find the determinent of the matrices.
]

#exercise(sub : "Polynomial")[
    Define `Polynomial` as a type alias for `[a]` where `a` is a number. Here $[2,3,1] |-> 2 + 3x +  x^2$, the reason for this will become evident soon.
    
    We want you to implement

    (i) `evaluateAt :: Polynomial a -> a -> a` which takes a polynomial and evaluates it at a given value.

    (ii) `addPoly :: Polynomial a -> Polynomial a -> Polynomial a` and `subPoly :: Polynomial a -> Polynomial a -> Polynomial a`.

    (iii) `degree :: Polynomial a -> Int` which returns the degree of the polynomial.

    (iii) `diffrentiate :: Polynomial a -> Polynomial a` which diffrentiates the polynomial and `integrate :: Polynomial a -> Polynomial a` which integrates the polynomial.

    (iv) `interpolate :: [Int] -> Polynomial Int` which takes a list of length $n$ say $l$ and outputs a degree $n-1$ polynomial $p$ such that $[p(0), p(1), dots, p(n-1)] = l$.
]

#exercise(sub : "Complex")[
    Define `Complex a` as a type alias for `(a,a)` where `a` is a number. We want you to evaluate

    (i) `addComp :: Complex a -> Complex a -> Complex a` which takes two complex numbers and adds them. Similerly, define `subComp ::  Complex a -> Complex a -> Complex a`.

    (ii) Define `mulComp :: Complex a -> Complex a -> Complex a` and `divComp :: (Fractional a) => Complex a -> Complex a -> Complex a`

    (iii) Using binary exponentiation, define `expComp :: Complex a -> Int -> Complex a` which takes a complex number and raises it to some integral power.

    (iv) Finally define `omegaTor :: Int -> Int -> Complex Double` which takes two integers $n,r$ and gives $omega^r$ where $omega$ is the $n$-th root of $1$.
]

#exercise(sub : "FFT")[
    To end this chapter will tackle one of the most famous and somewhat complicated algorithms of all time. You are expected to have implemented `Matrix`, `Polynomial` and `Complex`.

    The main use of FFT is to multiply polynomials. If we multiply polynomials naivly, we will end up making $binom(n,2)$ multiplications and then some additions. The idea is that a $n$ degree polynomial is determined by what value it takes for some $n+1$ inputs. 
    
    What if we choose some special input which the polynomial can evaluate quickly, evaluate the polynomials at thoose inputs and multiply the corresponding inputs? We just need to choose these special inputs carefully.

    #def(sub : "DFT")[
        The discrete fourier transform of a polynomial $p$ of degree $m-1$,
        $
        "DFT"(p) = [p(omega^0), p(omega^1), dots, p(omega^(m-1))]
        $
        where $omega$ is the $m$-th root of unity.
    ]
    As we can represent the polynomial as $a_0 + a_1 x + a_2 x^2 + dots + a_(m-1) x^(m-1)$, therefore we could represent the DFT as
    $
    mat(
        omega^0, omega^0, omega^0, dots, omega^0;
        omega^0, omega^1, omega^2, dots, omega^(m-1);
        omega^0, omega^2, omega^4, dots, omega^2(m-1);
        \u{22EE}, \u{22EE}, \u{22EE}, \u{22F1}, \u{22EE};
        omega^0, omega^(m-1), omega^(2(m-1)), dots, omega^((m-1)(m-1))
    )
    mat(
        a_0;
        a_1;
        a_2;
        \u{22EE};
        a_(m-1)
    )
    $
    This multiplication can be simplified by taking modulo $m$ of every exponent as $omega^m = 1 = omega^0$ by definition. We will now present an example for $m= 8$ and hope you will be able to generalize the strategy.
    #image("../images/FFT/8matrix.png")
    We can reorder the columns of matrix and the corresponding rows in the vector to bring the even-numbered columns to the left and the odd-numbered columns to the right.
    #image("../images/FFT/8matrixReordered.png")
    As the quadrents on the left are identical, we now only need to compute three products.
    #image("../images/FFT/8matrix3product.png")
    Here, the first product is litrally $"DFT"([a_0, a_2, a_4, a_6])$. We can calculate the other products by noticing
    #image("../images/FFT/8matrixObservation.png")
    Which means we only need to compute $"DFT"([a_1, a_3, a_5, a_7])$ and multiplying the respective values.

    Implement `dft :: Polynomial a -> [Complex Double]`. Keep in mind that as we are multiplying two polynomials of degree $m-1$, the output will be of degree atmost $2m - 2$. So we must choose which root of unity to take with that in mind.

    (ii) Now to implement `mulPoly :: Polynomial a -> Polynomial a -> Polynomial a`, we need a way to convert back from values at $omega^0, omega^1, dots omega^(n-1)$ to a polynomial of degree $n-1$.

    Prove $"DFT"("DFT"([a_0, a_1, dots, a_(m-1)])) = [m a_0, m a_(m-1), m a_(m-2), dots, m a_1]$

    (iii) Using the above fact, finally implement `mulPoly :: Polynomial a -> Polynomial a -> Polynomial a`.

    If you did this, pat yourself on the back. You are one of the few people in the world who will have ever hand written a FFT implementation.
]



// cite
// Haskell Mooc
// https://jameshaydon.github.io/passport/
// citation 2
