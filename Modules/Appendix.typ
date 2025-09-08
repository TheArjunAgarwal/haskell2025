#let appendixed(body) = context{
  
  let index = (
    query(
      selector(<appendixed_content_source>)
      .before( here() )
    )
    .len()
  )

  [
    #link( 
      label( "appendixed_content_destination_" + str( index ) ) , 
      [ appendix ]
    )
    #metadata(body) 
    <appendixed_content_source> 
  ]
}

#let appendix = context{

  let included_elements = query(
    (
      selector(heading)
      .or(<appendixed_content_source>)
    )
    .before( here() )
  )

  let index = 0
  
  for element in included_elements {

    if element.func() == heading {

      heading(
        depth:element.depth,
        outlined:false,
        bookmarked:element.bookmarked,
        element.body
      )

      link(element.location(),[- return to the above heading])
    
    }
  
    if element.func() == metadata {
      
      [\ 
        #link(
          element.location(),
          [- return to the context of the following content])
      \ ]
            
      [ #[] #label( "appendixed_content_destination_" + str( index ) ) ]

      element.value

      index += 1
      
    }
    
  } 
  
}