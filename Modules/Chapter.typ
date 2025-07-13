//---------------------functions----------

#let title_page(
  title : [Title] ,
  author : [Author]
) = [#figure(
  {

    set par( leading: 0.35em, justify: false)
    
    text( 
      font : "Libertinus Serif",
      size : 45pt , 
      { 
        set heading(numbering : it => {})
        heading( offset : 0 , depth : 1, title )
      }
    )

    text( 
      font : "Libertinus Serif",
      size : 16pt , 
      emph( author ) 
    )

    [\ \ ]
      
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

  set heading( offset : 1 )

  set page(
    header : align(right,{
      text( 
        font : "Libertinus Serif",
        weight : "extrabold" , title 
      )
      // [ -- ]
      // author
    })
  )
user_end_body}

//------------------tests---------------------

#settings(title : [Title] , author : [Author])[
  #title_page()
]
