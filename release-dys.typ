#{
  
import "/Modules/Book-Dys.typ"
import "/Modules/Box.typ"
import "/Modules/Chapter.typ"
import "/Modules/Code.typ"
import "/Modules/Contents-Dys.typ"
import "/Modules/Prelude.typ"

let title = [Haskell for CMI]

let R = [Ryan Hota]
let S = [Shubh Sharma]
let A = [Arjun Maneesh Agarwal]
let author = R+[, ]+S+[, ]+A

show: Book-Dys.settings.with(title:title,author:author)
show: Box.settings
show: Code.settings
show: Contents-Dys.settings
show: Prelude.settings

Book-Dys.title_page(title:title,author:author)

set page(numbering: "i") ; counter(page).update(1)

Book-Dys.copyright_declaration(title:title,author:author)

Book-Dys.dedication([To someone])

Contents-Dys.contents

set page(numbering: "1") ; counter(page).update(1)

counter(heading).update(0)

let chapters = (
  (
    "ch01_functions.typ",
    [Basic Theory],
    author
  ),
  (
    "ch02_setup.typ",
    [Installing Haskell],
    author
  ),
  (
    "ch03_datatypes.typ",
    [Basic Syntax],
    A
  ),
  (
    "ch04_tuples.typ",
    [Types as Sets],
    R
  ),
  (
    "ch05_lists.typ",
    [Introduction to Lists],
    R
  ),
  // (
  //   "ch06_polymorphism.typ",
  //   [Polymorphism and Higher Order Functions],
  //   S
  // ),
  // (
  //   "ch07_advanced_lists.typ",
  //   [Advanced List Operations],
  //   A
  // ),
  // (
  //   "ch08_precomp-datatypes.typ",
  //   [Introduction to Datatypes],
  //   S
  // ),
  // (
  //   "ch09_computation.typ",
  //   [Computation as Reduction],
  //   S
  // ),
  // (
  //   "ch10_complexity.typ",
  //   [Complexity],
  //   A
  // ),
  // (
  //   "ch11_postcomp-datatypes.typ",
  //   [Advanced Data Structures],
  //   A
  // ),
  // (
  //   "ch12_typeclasses.typ",
  //   [Type Classes],
  //   R
  // ),
  // (
  //   "ch13_monad.typ",
  //   [Monads],
  //   R
  // ),
  // (
  //   "../extra-typ/appendix.typ",
  //   [Appendix],
  //   author
  // )
)

for chapter_data in chapters {

  pagebreak()

  show: Chapter.settings.with(
    title: chapter_data.at(1) ,
    author: chapter_data.at(2) ,
  )
  
  Chapter.title_page(
    title: chapter_data.at(1) ,
    author: chapter_data.at(2)
  )

  include "/chapters-typ//" + chapter_data.at(0)

}
}