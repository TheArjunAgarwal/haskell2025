#import "Prelude.typ" : targets , current

//--------------------functions-----------------------

#let contents_selector = {
  figure.where( kind : "chapter_title" )
  .or( heading.where( outlined : true )
  .or( figure.where( kind : "fancy_box" ) ) )
}

#let contents = pagebreak() + {

  figure( 
    text(size : 2em , 
      heading( 
        level : 1 , 
        outlined : true , 
        [Table of Contents] 
      ) 
    ) ,
    kind : "contents_title" , 
    supplement : none
  ) 
  
  [\ ]
  
  show outline.entry : it => figure(align(center,{
    
    if targets( it , "chapter_title" ) [
      #set text( size : 1.4em , weight: "extrabold" )
      #v( 2em )
      #link( 
        it.element.location() , 
        it.element.supplement 
      )
      #box( width: 1fr , repeat(" ") ) 
      #text( weight : "extrabold" , it.page() )
      #v( 0.25em )
    ]

    if it.element.func() == heading {
      v(0.2em)
      set text( size : 1.3em )
      it.indented(
        it.prefix() , 
        link( it.element.location() , it.body() )
        + " " 
        + box( width : 1fr , repeat(gap : 0.15em)[.] ) 
        + " "
        + it.page()
        + h( ( it.level - 1 ) * 1em )
      )
    }
    
    if targets( it , "fancy_box" ) {
      v(0.1em)
      align( center , box( width : 50% , 
        link( 
          it.element.location() , 
          it.element.supplement 
        )
        + text( fill : gray , it.inner() )
      ))
    }

    // if it.level == 1 and targets( it , "fancy_box" ) {
    //   let fancy_level = 1 + query(heading.where(outlined:true)).map(h=>h.level).sorted().last()
    //   outline.entry( fancy_level , it.element , fill : it.fill )
    // }
    
  }) ,
  kind : "contents_entry" ,
  supplement : none
  )

  outline( 
    title : none , 
    target : contents_selector , 
    indent : 2em
  )
  
}

//------------------settings-----------------------------

#let settings( user_end_body ) = {

  set page( foreground: {
    
    set align(left + top) ; v(1em) ; h(1em)
    
    context{

      let number_of_contents_targets_past = {
        query( contents_selector )
        .filter( it => it.location().page() < here().page() )
        .len()
      }
      
      let number_of_contents_targets_present = {
        query( contents_selector )
        .filter( it => it.location().page() == here().page() )
        .len()
      }

      let current_number_of_contents_targets = number_of_contents_targets_past + ( number_of_contents_targets_present.signum() )

      let corresponding_contents_entry = {
        ( 
          query( figure.where( kind : "contents_title" ) )
          + query( figure.where( kind : "contents_entry" ) )
        )
        .at( current_number_of_contents_targets )
      }

      let destination = corresponding_contents_entry.location()

      let symbol = { text(size:0.5em,fill:blue)[#underline[Contents]]

        // if here().page() < destination.page() [after]

        // if here().page() == destination.page() [here]

        // if here().page() > destination.page() [before]
        
      }

      link( destination , symbol )
      
    }
  
  })

    
user_end_body}
  