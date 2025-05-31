//---------------------functions--------------------

#let light( colour ) = color.mix( (colour, 25%) , white )

#let inline_box( 
  colour : black , 
  body 
) = {
  box(
    height : 1.3em ,
    inset : 3em/11 ,
    radius : 3em/11 ,
    baseline : 2.5em/11 ,
    fill : light( colour ) ,
    text(
      baseline: 0.06em,
      body
    )
  )
}

#let ordinary_box( 
  colour : blue , 
  body 
) = figure(
  box( 
    fill : light(colour) , 
    width : 95% ,
    inset: 6pt , 
    radius: 4pt ,
    { 
      set align(start) 
      body 
    }
  )
  , kind : "ordinary_box"
  , supplement : "Box"
  , numbering : none
)

#let fancy_title( 
  colour : black , 
  symbol : [\$], 
  title , 
  fancy_title_size : 1em
) = {

  box[
    #set text( fill : colour )
    #box[ 
      #circle( 
        fill: light(colour) , 
        radius : fancy_title_size/2 
      )[
        #set align( center + horizon )
        #text( 
          size : 0.7*fancy_title_size , 
          weight: "extrabold" ,
          symbol.body 
        )
      ]
  
    ]
    #h( weak: true , fancy_title_size/3 )
    #text( 
      size : fancy_title_size , 
      weight : "extrabold" , 
      baseline : -fancy_title_size/11 , 
      title
    )
  ] 
}

#let fancy_box( 
  colour : black , 
  symbol : $\$$ , 
  title : none , 
  fancy_title_size : 1em , 
  label_prefix : "fancy box containing" , 
  outlined : true ,
  body 
) = [
  
  #if title!=none {

    v( fancy_title_size , weak :true )
    
    [
    
    #figure( 
      [
      #set align( start )
  
      #fancy_title( 
        colour : colour , 
        symbol : symbol , 
        title , 
        fancy_title_size : fancy_title_size )
      
      #v( fancy_title_size/6 , weak : true )
      
      #ordinary_box( colour : colour , body ) 
      ]
      ,
      kind : (if outlined {""} else {"unoutlined_"} ) + "fancy_box",
      supplement : fancy_title( 
        colour : colour , 
        symbol : symbol , 
        title , 
        fancy_title_size : fancy_title_size )
    )
    
    #label( ( label_prefix + " " +  title ).replace( " " , "_" ) )

    ]
    
  } else {

    ordinary_box( colour : colour ,  body )
    
  }  
  
]

//---------------------settings------------------------

#let settings( user_end_body ) = {

import "Prelude.typ" : *
 
show ref : it => if targets( it , "fancy_box" ) or targets(it, "unoutlined_fancy_box") {  
  box( baseline : 0.1em , 
    link( it.target , it.element.supplement )
  )
} else { it }

user_end_body}

//---------------------tests------------------------

// #settings[

// In the #inline_box([Middle]) of the line.
// #fancy_box([Body])
// #fancy_box(title : "Title", [Body])
// Let me refer to @fancy_box_containing_Title.

// ]
