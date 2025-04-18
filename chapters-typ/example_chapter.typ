= Code

In the `Middle` of a line.

// hidden from none
```haskell
-- hello1
hello1 :: any -> String
hello1     _  =  "world"
```

Refer to @code_of_hello1.

// hidden from contents
```haskell
hello2 :: any -> String
hello2     _  =  "world"
```

// hidden from lhs
```
-- hello3
hello3 :: any -> String
hello3     _  =  "world"
```

Refer to @code_of_hello3.

// hidden from contents, typ
#metadata[
```haskell
-- hello4
hello4 :: any -> String
hello4     _  =  "world"
```
]

// hidden from contents, lhs
```
hello5 :: any -> String
hello5     _  =  "world"
```

//hidden from contents, lhs, typ
#metadata[
```
-- hello6
hello6 :: any -> String
hello6     _  =  "world"
```
]

= Definition 

#import "../Definition.typ" : def

// hidden from none
#def( subject : "disjoint union" )[ 
  I'm defining _$X union.sq Y$_ here, \
  which is not *$X union Y$* exactly.
]

Refer to @definition_of__disjoint_union.

// hidden from contents
#def[ 
  Again, I'm defining _$X union.sq Y$_ here, \
  which is not *$X union Y$* exactly.
]

