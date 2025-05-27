  //---------------functions-----------------

#let title_page(
  title : [Title] ,
  author : [Author]
) = {

  set par( leading : 3.5em )
  
  set page( header : none )
  
  set align( center + horizon )
  
  text( 
    size : 100pt , 
    weight : "extrabold" , 
    title 
  ) + [\ ]
  
  text( 
    size : 20pt , 
    emph( author ) 
  )
    
}

#let copyright_declaration( 
  title : [Title] , 
  author : [Author] , 
  text_license : [CC-by-SA-4.0] , 
  code_license : [AGPL-3.0] ,
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

  if text_license != none {
    "Text licensed under " + text_license + [\ ]
  }

  if code_license != none {
    "Code licensed under " + code_license + [\ ]
  }

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
    theme : auto //"theme.tmTheme"
  ) ,
  user_end_body
) = {

  set page(
    header : align(right)[
      #text( 
        weight : "extrabold" , title 
      )
      --
      #author
    ]
  )
  
  set raw( theme : code_text_settings.theme )
  
  show raw : it => {
    set text(
      size : code_text_settings.size,
      font : code_text_settings.font,
      stylistic-set : code_text_settings.stylistic-set 
    )
    it
  }
    
  // because heading levels were to be changed to fixed metadata index
  show heading.where(level: 2): set text(size: 16pt)
  show heading.where(level: 3): set text(size: 14pt)
  show heading.where(level: 4): set text(size: 12pt)

  user_end_body
}

//---------tests--------------------------

#title_page()
#copyright_declaration()
#dedication[To someone]
