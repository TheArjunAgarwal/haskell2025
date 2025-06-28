#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate

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

The purpose of having sum types is to have a collection of many possible _variants_ in a type. This is similar to what we did with #link(<fin>, "Finite types") but we can have an entire collection as a variant with the help of #link(<para>)[Parametric types].

Here are some examples:
```
IntOrString = I Integer | S String 

Maybe a = Just a | Nothing    -- This is how Haskell defines Maybe types!
Either a b = Left a | Right b -- This is how Haskell defines Either types!

Shape = Circle { radius :: Integer } -- Record syntax works!
      | Square { side :: Integer }
      | Rectangle { len :: Integer, width :: Integer }
```

Just like finite types, to define a function on a sum type, one needs to define it on all variants. This is also called Pattern Matching!

#exercise(sub : "Better Dating Profile")[
Think of some interesting questions and possible answers for those questions / information bits for a dating profile and incorporate it into you `Profile` type.
]

#line(length: 100%)

- Define recurssion in recursive data types and define (4)
- define Nat, List, Tree
// Chapter content goes here

// cite
// Haskell Mooc
// citation 2
In #link(<types>)[Chapter 4], we saw that types can be thought of as sets, and given simple types one can put them together in multiple ways to form more sophisticated types, some of the options we have dicussed so far:
- We can take @definition_of_cartesian_product of types `A` and `B` as `(A, B)`.
- We can take @definition_of_disjoint_union of types `A` and `B` as `Either A B`.
- We can create the list of elements of type `A` as `[A]`.
And so on...

