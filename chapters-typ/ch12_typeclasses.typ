#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate

= typeclasses (feel free to change it)

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

On one hand, this seems like a fair thing to ask, as we already have a definition for equality of mathematical functions, on the other hand we run into 2 issues:
- Is it really fair to say that? In computer science, the way things are computed matter, hence the name of the entire field. I lot of times, one will be able to distinguish which of the functions are running, by simply looking at which one works faster or slower on big inputs, and that might be something people might want to factor in what the mean by "sameness". So maybe the assumption that 2 functions being equal pointwise imply the functions are equal may not be wise.
- The second is that in general it is not possible, in this case we have a mathematical identity that lets us prove so, but given any 2 function, it might be that the only way to prove that they are equal would be to actually check on every single value, and since domains of functions can be infinite, this would simply not be possible to compute.

So we can't have the type of *`(==)`* be `a -> a -> Bool`. In fact, if I try to write it, the haskell compiler will complain to me by saying

```
test.hs:8:7: error: [GHC-39999]
    • No instance for ‘Eq (Int -> Int)’ arising from a use of ‘==’
      ... more error
  |
8 | h = f == g
  |   
```

To tackel this, we define the following:

#def(sub: "Typeclasses")[
_Typeclasses_ are a collection of types, characterizede by their common _shape_.
]

The previous section describes how one writes functions based on the _shape_ of the objects. And that different types can have some aspects of their _shape_ in common. And @code_of_Function_Extensionality tells us that we need to be careful, that common shape might not be present in all types.

Typeclasses are how one expresses in haskell, what a collection of types looks like and what exactly is the common _shape_, equivalently, what functions can be defined over the entire class. Some examples are: 
- `Eq`, which is the collection of all types for which the function *`(==)`* is defined.
- `Ord`, which is the collection of all types for which the function *`(<)`* is defined.
- `Show`, which is the collection of all types for which there is a function that converts them to `String` using the function *`show`*.

Note that in the above cases, defining one function lets you define some other functions, like *`(/=)`* for `Eq` and *`(<=)`*, *`(>=)`* and so on for the `Ord` typeclass.

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

But if we do that we will encounter the same issue as we did in @code_of_Function_Extensionality, because of `(==)` we need to find a way to say that `a` belongs to the collection `Eq`, and this leads to the correct type:
```
elem :: Eq a => a -> [a] -> Bool
```
#exercise(sub: "Checking if a list is sorted")[
Write the function `isSorted` which takes in a list as an argument, such that the elements of the list have a notion of ordering between them, and the output should be true if the list in an ascending order (equal elements are allowed to be next to each other), and false otherwise.
]


// Chapter content goes here

// cite
// citation 1
// citation 2
