//---------------functions-----------------

#let title_page(
  title : [Title] ,
  author : [Author]
) = {
  
  set align( center + horizon )
  text( 
    size : 100pt , 
    weight : "extrabold" , 
    title 
  ) + [\ \ ]
  
  text( 
    size : 20pt , 
    emph( author ) 
  )
    
}

#let copyright_declaration( 
  title : [Title] , 
  author : [Author] , 
  license : [CC-by-SA-4.0] , 
  draft : true , 
  feedback_host : [feedback_host\@mailthing.com] 
) = {

  pagebreak()

  set align( center + top )

  import datetime : *
  
  box( baseline: 0.1em )[ 
    #circle( 
      inset : 0.07em , 
      align( 
        center + horizon , 
        text( 
          size : 0.7em , 
          weight : "bold", 
          [C] 
        ) 
      ) 
    ) 
  ] 
  " "  
  [#today().year()]
  " "
  author + [\ ]

  "Text licensed under " + license + [\ ]

  if draft [This is (still!) an incomplete draft.\ ]

  [Please send any corrections, comments etc. to ] + feedback_host + [\ ]

  [#today().display("Last updated [month repr:long] [day], [year].")]
  
}

#let dedication( 
  dip : 11 , 
  size : 22pt , 
  body 
) = [
  
  #pagebreak()

  #for _ in dip*(none,) {text( size : 22pt , [\ ] )}
  
  #set align( center + horizon )

  #set text( size : size , style: "italic")

  #body
  
]

//-------------------settings---------------

#let settings(
  title : [Title] ,
  author : [Author] ,
  code_text_settings : ( 
    size : 1.3em, 
    font : "Fira Code" , 
    stylistic-set: (3,4,5,6,9) ,
    theme : "theme.tmTheme"
  ) ,
  user_end_body
) = {

  set page(
    header : align(right)[
      #text( 
        weight : "extrabold" , title 
      )
      --
      Author
    ]
  )

  set raw( theme : "theme.tmTheme" )
  
  show raw : it => {
    set text(
      size : code_text_settings.size,
      font : code_text_settings.font,
      stylistic-set : code_text_settings.stylistic-set 
    )
    it
  }

user_end_body}

//---------tests--------------------------

#title_page()
#copyright_declaration()
#dedication[To someone]
