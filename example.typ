#{
  
import "Book.typ"
import "Box.typ"
import "Chapter.typ"
import "Code.typ"
import "Contents.typ"
import "Definition.typ" : *
import "Exercise.typ": *
import "Prelude.typ"

let title = [Example Title]
let author = [Example Author]

Book.settings(title:title,author:author,{
Box.settings({
Code.settings({
Contents.settings({
Prelude.settings({

Book.title_page(title:title,author:author)

set page(numbering: "i") ; counter(page).update(1)

Book.copyright_declaration(title:title,author:author)

Book.dedication([To someone])

Contents.contents

set page(numbering: "1") ; counter(page).update(1)

let chapters = (
  ( "example_chapter.typ" , [Example Chapter Title] , [Example Chapter Author] ) ,
)

for chapter_data in chapters {

  pagebreak()
  
  Chapter.title_page(
    title: chapter_data.at(1) ,
    author: chapter_data.at(2)
  )

  Chapter.settings(
    title: chapter_data.at(1) ,
    author: chapter_data.at(2) ,
    include "chapters-typ//" + chapter_data.at(0)
  )

}

})
})
})
})
})

}
