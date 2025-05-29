#import "Box.typ" : fancy_box

//-----------------functions-----------------------

#let definition_colour = color.linear-rgb(50%,10%,30%)


#let def( 
  definition_colour : definition_colour , 
  sub : none , 
  body 
) = {

  let definition_text( body ) = text( 
    fill : definition_colour , 
    body 
  )
  
  fancy_box(
    colour : definition_colour,
    symbol : $eq.dots$,
    title : sub,
    label_prefix : "definition of",
    { 
      show emph : strong
      show strong : definition_text
      body 
    }
  )
}

//-----------------------tests----------------------

// hidden from none
#def( sub : "disjoint union" )[ 
  I'm defining _$X union.sq Y$_ here, \
  which is not *$X union Y$* exactly.
]

// hidden from contents
#def[ 
  Again, I'm defining _$X union.sq Y$_ here, \
  which is not *$X union Y$* exactly.
]