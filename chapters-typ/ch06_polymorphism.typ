#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote

#let definition = def
#let example = it => [For example - \ #it]

// Chapter content goes here

= Polymorphism and Higher Order Functions

== Polymorphism

Functions are our way, to interact with the elements of a type, and one can define functions in one of the two following ways:
+ Define an output for every single element.
+ Consider the general shape and behaviour of elements, and how they interact with other simple functions to build more complex function.

Up until the section about lists, we saw how to define functions from a given type, to another given type, for example:

*`nand`* is a function that accepts 2 *`Bool`* values, and checks it at least one of them is *`False`*. We will show two ways to write this function.drop

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

But with the addition of the List type in the previous chapter, we were able to add _new_ information to a type. In the following sense:

Consider the type *`[Int]`*, the elements of these types are lists of integers, the way one would interact with these would be to treat it as a collection of objects, in which each element is an integer.
- so to write a function for this type, one first needs to think about the fact that the _shape_ of an element looks like a list, and how one gets to the items of the list, and then treat the items like integers and write functions on them.
- A function for lists would thus have 2 components, at least conceptually if not explicit in code itself, consider the following example:
  ```
  squareAll :: [Int] -> [Int]
  squareAll []       = []
  squareAll (x : xs) = x * x : squareAll xs  
  ```
  Here, in the definition when we match patterns, we figure out the shape of the list element, and if we can extract an integer from it, we do so, square it, then put it back in the list.

Something similar can be done with the type *`[Bool]`*:
- Once again, to write a function, one needs to first look at the _shape_ an element as a list, Then pick elements out of them and treat them as *`Bool`* elements.
- An example of this will be the *`and`* function, that takes in a collection of *`Bool`* and returns *`True`* if and only if all of them are *`True`*.
  ```
  and :: [Bool] -> Bool 
  and []       = True  -- We call scenarios like this 'vacuously true'
  and (x : xs) = x && and xs
  ```
  Once again, the pattern matching handles the shape of an element as a list, and the definition handles each item of a list as a *`Bool`*.

Then we see functions like the following: 
- *`drop`*, which takes a list and discards the first couple of elements as specified
- *`elem`*, which checks if an element belongs to a list
- *`(==)`*, which checks if 2 elements are the same

Up until now, we had been emphasizing on the _shape_ of elemets of a type, but these functions don't seem to care about it that much:
- The *`drop`* function just cares about the list structre of an element, and not what the internal item looks like.
- The *`elem`* function also doesn't care about the internal type as long as there is some notion of equality defined.
- The *`(==)`* works on all types where some notion of equality is defined (A counter example would be the type of functions: *`Int -> Int`*).

Now one can define such functions for every single type, but that has 2 problems:
- The first is that the defintion of all of these functions is the exact same, so doing this would be a lot of manual work, and one would also need to have different name for different types, which is very inconvenient.
- The second, and arguably a more serious issue, it stops us from abstracting, abstraction is the process of looking at a scenario and removing information that is not relevant to the problem. 
    - An example would be that the `drop` simply lets us treat elememts as lists, while we can ignore the type of items in the list.
    - All of Mathematics and Computer Science is done like this, in some sense it is just that.
        - Linear Algebra lets you treat any set where addition and scaling is define as one _kind_ of thing.
        - Metric Spaces let us talk about all sets where there is a notion of distance.
        - Groups let us talk about sets where there is a notion of "combining" things together with more restriction.
    - And this is a powerful tool because solving a problem in the _abstract_ version solves the problem in all _concretized_ scenarios.
    
#quote(subject : "John Locke, An Essay Concerning Human Understanding (1690)")[
The acts of the mind, wherein it exerts its power over simple ideas, are chiefly these three: 1. Combining several simple ideas into one compound one, and thus all complex ideas are made. 2. The second is bringing two ideas, whether simple or complex, together, and setting them by one another so as to take a view of them at once, without uniting them into one, by which it gets all its ideas of relations. 3. The third is separating them from all other ideas that accompany them in their real existence: this is called *abstraction*, and thus all its general ideas are made. 
]

One of the ways abstraction is handled in Haskell, and a lot of other programming languages is *Polymorphism*.

#def(subject : "Polymorphism")[
A *polymorphic* function is one whose output type depends on the input type. Such a property of a function is called *polymorphism*, and the word itself is latin for _many forms_.
]

A polymorphic function differs from functions we have seen in the following ways:
- It can take input from multiple differnt input types (not necessarily type, restrictions are allowed).
- Its output type can be differnt for different inputs.

An example for such a function that we have seen in the previous section would be:
```
drop :: Int -> [a] -> [a]
drop _ []     = [] 
drop 0 ls     = ls
drop n (x:xs) = drop (n-1) xs
```
The polymorphism of this function is shown in the type *`drop :: Int -> [a] -> [a]`* where we have used variables (usually called a type variable) instead of explicity mentioning a types, this still has a lot of structure, and is not the same as forgetting about types, for instance, the same variable is used in both the second argument and the output, so they need to be of the same type, dropping some elements from a list of integers also gives a list of integers, we still have all the safety and correctness guarantees that types give us.

#exercise(subject: "Datatypes of some list functions")[
A nice excerise would be to write the types of the following functions defined in the previous section: *`head`*, *`tail`*, *`(!!)`*, *`take`* and *`splitAt`*.
]

=== A Taste of Type Classes
Consider the case of the integer functions

```
f :: Int -> Int
f x = x^2 + 2*x + 1

g :: Int -> Int 
g x = (x + 1)^2
```

We know that both functions, do the same thing in the mathematical sense, given any input, both of then have the same output. But the does the following expression make sense in haskell?

```
-- | Functions being equal?
f == g
```

On one hand, this seems like a fair thing to ask, as we already have a well defined mathematical function, on the other hand we run into 2 issues:
- Is it really fair to say that? In computer science, the way things are computed matter, hence the name of the entire field. I lot of times, one will be able to distinguish which of the functions are running, by simply looking at which one works faster or slower on big inputs, and that might be something people might want to factor in what the mean by "sameness". So maybe the assumption that 2 functions being equal pointwise imply the functions are equal may not be wise.
- The second is that in general it is not possible, in this case we have a mathematical identity that lets us prove so, but given any 2 function, it might be that the only way to prove that they are equal would be to actually check on every single value, and since domains of functions can be infinite, this would simply not be possible to compute.

So we can't have the type of *`(==)`* be `a -> a -> Bool`. 

This is where we introduce *Typeclasses*. We will be looking at type classes in to much more detail in a later chapter, but for now.

#def(subject: "Typeclasses")[
Typeclasses are a collection of types, characterizede by their behaviour.
]

In the above definition, by 'behaviour' we mean how types interact with other types using functions, that is, what functions are allowed to a the given input type.Some examples of typeclasses are:
- `Eq`, which is the collection of all types for which the function *`(==)`* is defined.
- `Ord`, which is the collection of all types for which the function *`(<)`* is defined.
- `Show`, which is the collection of all types for which there is a function that converts them to `String` using the function *`show`*.

Note that in the above cases, defining one function lets you define some other functions, like *`(/=)`* for `Eq` and *`(<=)`*, *`(>=)`* and so on for the `Ord` typeclass.

We will look into how they work, and how to define your own typeclass in a later section, but for now, we use these to define better polymorphic functions.

```
elem :: Eq a => a -> [a] -> Bool
elem _ []       = False
elem e (x : xs) = e == x || elem e xs
```
Note that there is a use of *`(==)`* in the last line of the defintion, this means that we must add the part `Eq a =>` in our type, this is what tells haskell that the input type for this function should be anything that implements `Eq`. 

In fact, don't add it, the compiler yells at us by saying:
```
elem.hs:5:21: error: [GHC-39999]
    • No instance for ‘Eq a’ arising from a use of ‘==’
      ... (more error)
  |
5 | elem e (x : xs) = e == x || elem e xs
  |                     ^^
Failed, no modules loaded.
```

To put more emphasis on how type classes do not come at a cost of safety, if we had a list of function, and we passed it to `elem` the code simply does not compile saying the input type is incorrect.

#exercise(subject : "Checking if a list is sorted")[
Write the function `isSorted` which takes in a list as an argument, such that the elements of the list have a notion of ordering between them, and the output should be true if the list in an ascending order (equal elements are allowed to be next to each other), and false otherwise.
]

#linebreak()


== Higher Order Functions

// cite 
// citation 1
// citation 2
