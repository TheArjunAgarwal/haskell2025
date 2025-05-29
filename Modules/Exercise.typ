#import "Box.typ" : fancy_box

#let exercise(sub: "Exercise" , body ) = {
  fancy_box( 
    colour : blue , 
    symbol : $X$ , 
    title : sub, 
    fancy_title_size : 1em , 
    label_prefix : "exercise of" , 
    outlined : false,
    body 
  )
}