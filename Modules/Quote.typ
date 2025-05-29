#import "Box.typ" : fancy_box

#let quote_colour = color.linear-rgb(40%,27%,27%)

#let quote(sub: none , body ) = {
  fancy_box( 
    colour : quote_colour , 
    symbol : $bar.v.circle$ , 
    title : sub, 
    fancy_title_size : 1em , 
    label_prefix : "quote of" , 
    outlined : false,
    body 
  )
}
