#import "style.typ":*
#show: codly-init.with()
#codly(languages: codly-languages)

#show: main.with(
  title: [Introduction to Programming in Haskell],
  subtitle: [Notes for CMI's CU1101 course in Winter 2025],
  author: "Ryan Hota, Shubh Sharma, Arjun Maneesh Agarwal",
  date: datetime.today(),
  report-style: true,
)


#pagebreak()

#epigraph("Programs must be written for people to read, and only incidentally for machines to execute.", "Harold Abelson & Gerald Jay Sussman, Structure and Interpretation of Computer Programs")
// We must(!) choose something else later. This is just a placeholder

#pagebreak()

#dedication("To cats and dogs")
// We must(!) choose something else later. This is just a placeholder

#copyright("Ryan Hota, Shubh Sharma, Arjun Maneesh Agarwal", "CC-by-SA-4.0", "arjuna.ug2024@cmi.ac.in")

#toc

#pagebreak()

#include "extra-typ/preface.typ"
#pagebreak()
#include "chapters-typ/ch01_functions.typ"
#pagebreak()
#include "chapters-typ/ch02_setup_linux.typ"
#include "chapters-typ/ch02_setup_mac.typ"
#include "chapters-typ/ch02_setup_win.typ"
#pagebreak()
#include "chapters-typ/ch03_datatypes.typ"
#pagebreak()
#include "chapters-typ/ch04_tuples.typ"
#pagebreak()
#include "chapters-typ/ch05_lists.typ"
#pagebreak()
#include "chapters-typ/ch06_polymorphism.typ"
#pagebreak()
#include "chapters-typ/ch07_advanced_lists.typ"
#pagebreak()
#include "chapters-typ/ch08_computation.typ"
#pagebreak()

#part[Appendix]
#include "extra-typ/appendix.typ"