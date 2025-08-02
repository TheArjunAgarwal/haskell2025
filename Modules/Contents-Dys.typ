#import "Prelude.typ" : targets , current

//--------------------functions-----------------------

#let contents_selector = {
  heading.where( outlined : true )
  .or( figure.where( kind : "fancy_box" ) )
}

#let contents = pagebreak() + {

  figure( 
    text(size : 2em , 
      {
        set heading(numbering : it => {})
        heading( 
          level : 1 , 
          outlined : true , 
          [Table of Contents] 
        )
      }
    ) ,
    kind : "contents_title" , 
    supplement : none
  ) 

  [\ ]

  show outline.entry : it => figure(align(center,{

    let left_indent_correction_term = -2.7em

    if it.element.func() == heading {

      if it.element.offset==0 {
        set text( size : 1.4em , weight: "extrabold" )
        v( 2em )
        link( 
          it.element.location() , 
          it.body()
        )
        box( width: 1fr , repeat(" ") ) 
        text( weight : "extrabold" , it.page() )
        v( 0.25em )

      }else{

        v(0.2em)
        set text( size : 1.3em )
        let b4_correction_entry = it.indented(
            it.prefix() , 
            link( it.element.location() , it.body() )
            + " " 
            + box( width : 1fr , repeat(gap : 0.15em)[.] ) 
            + " "
            + it.page()
            + h( ( it.level - 2 ) * 1em )
          )
        /*repr*/(block(
          inset: (
            left : 0.7*b4_correction_entry.inset.left + left_indent_correction_term
          ),
          b4_correction_entry.body
        ))

      }
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

  }) ,
  kind : "contents_entry" ,
  supplement : none
  )

  outline( 
    title : none , 
    target : contents_selector , 
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

  }, fill: rgb("FDF6E3")
  )

  set text(fill: rgb("375E97"))

  user_end_body}