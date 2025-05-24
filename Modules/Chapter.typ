//---------------------functions----------

#let title_page(
  title : [Title] ,
  author : [Author]
) = align( center + horizon )[#figure(
  {

    set par( leading: 2.5em)
    
    text( 
      size : 80pt , 
      weight : "extrabold" , 
      title 
    ) + [\ \ ]
    
    text( 
      size : 16pt , 
      emph( author ) 
    )
      
  } ,
  kind : "chapter_title" ,
  supplement : title
)]

//--------------------settings-------------

#let settings(
  title : [Title] ,
  author : [Author] ,
  user_end_body , 
) = {

  set page(
    header : align(right,{
      text( 
        weight : "extrabold" , title 
      )
      [ -- ]
      author
    })
  )
user_end_body}

//------------------tests---------------------

#settings(title : [Title] , author : [Author])[
  #title_page()
]