#import "Prelude.typ" : targets

#let contents_selector = figure.where(kind:"chapter_title").or(heading)

#let contents = {
  
  pagebreak()
  
  outline( target : contents_selector )

}

#let settings( user_end_body ) = {
  
  show outline.entry : it => {
    if targets( it , "chapter_title" ) {
      set text( size : 1.1em , weight: "extrabold" )
      v( 1em )
      link( 
        it.element.location() , 
        it.element.supplement 
        + box( width: 1fr , repeat(" ") ) 
        + text( weight : "extrabold" , it.page() )
      )
    } else { it }
  }
    
user_end_body}
  
