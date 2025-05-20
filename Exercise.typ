#import "Box.typ" : fancy_box

#let exercise( subject : none , body ) = {
  fancy_box( 
    colour : blue , 
    symbol : $X$ , 
    title : subject, 
    fancy_title_size : 1em , 
    label_prefix : "exercise of" , 
    outlined : false,
    body 
  )
}
