#import "Box.typ" : fancy_box

#let quote_colour = color.linear-rgb(40%,27%,27%)

#let quote( subject : none , body ) = {
  fancy_box( 
    colour : quote_colour , 
    symbol : $bar.v.circle$ , 
    title : subject, 
    fancy_title_size : 1em , 
    label_prefix : "quote of" , 
    outlined : false,
    body 
  )
}
