#import "Box.typ" : fancy_box , inline_box

//---------------------functions--------------------

#let code_colour = color.linear-rgb(30%,50%,10%)

#let unligate(body) = text(features: (calt : 0), body)


//-------------------settings-----------------------

#let settings( 
  code_colour : code_colour , 
  user_end_body 
) = {

show raw.where( 
  lang : none , 
  block : false 
) : code_display => {
  inline_box( 
    colour : code_colour , 
    raw( lang : "haskell" , code_display.text )
  ) 
}

show raw.where( block : true ) : code_display => {

  if (none,"haskell").contains(code_display.lang) {

    let haddock_str = "-- | "

    let code_title_and_str = code_display.text

    let code_title = if code_title_and_str.starts-with(haddock_str) {
      code_title_and_str
      .split("\n")
      .at(0)
      .trim(
        haddock_str , 
        at : start , 
        repeat : false
      )
      .trim()
    } else { none }

    let code_str = if code_title != none {
      code_title_and_str
      .trim(
        code_title_and_str.split("\n").at(0) , 
        at : start , 
        repeat : false 
      )
      .slice(1)
    } else { code_title_and_str }
    
    show raw : it => {
      fancy_box( 
        colour : code_colour, 
        symbol : $lambda$, 
        title : code_title , 
        label_prefix : "code of" ,
        it
      )
    }

    raw( lang : "haskell" , code_str )

  } else {

    code_display

  }

} 

user_end_body}

//-------------------tests-----------------------------

#show raw : it => { 
  set text( 
    size : 1.3em, 
    font : "Fira Code" , 
    stylistic-set: (3,4,5,9),//6 makes backslash super thin
    features: ("cv09",)
  )
  it 
}

#settings[



In the `Middle` of a line.

// hidden from none
```haskell
-- | hello1
hello1 :: any -> String
hello1     _  =  "world"
```

@code_of_hello1

// hidden from contents
```haskell
hello2 :: any -> String
hello2     _  =  "world"
```

// hidden from lhs
```
-- | hello3
hello3 :: any -> String
hello3     _  =  "world"
```

// hidden from contents, typ
#metadata[
```haskell
-- | hello4
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
-- | hello6
hello6 :: any -> String
hello6     _  =  "world"
```
]

]
