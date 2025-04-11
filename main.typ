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


#toc

#pagebreak()

// #include "extra-typ/preface.typ"
// #pagebreak()
#include "example.typ"
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