#let body = [
  #show heading.where( depth: 1): it => [ #pagebreak() #it ]
  #show heading.where( depth: 2): it => [ #pagebreak() #it ]

  = Chapter Sketches

== ✅ Chapter 1: Function Definitions & Flow Control

- *Topics*: Function definition, pattern matching, recursion, induction, `let`, `where`, `if-then-else` as flow control.
- *Time*: 1 Class + 1 Tutorial  
- *Author*: RSA  
- *Notes*: No polymorphism; fully pen-and-paper before code.

---

== 🛠 Chapter 2: Haskell Setup

- *Topics*: Minimal setup (hopefully using haskellKISS) for different OS.
- *Time*: 1 Parallel Tutorial  
- *Authors*:  
  - *Windows*: R  
  - *Linux*: S  
  - *macOS*: A  

---

== 🔤 Chapter 3: Basic Datatypes

- *Topics*:  
  - `Bool`, `Int` vs `Integer` in extremely brief terms, `Char` (`ord`, `chr`)  
  - Use of `.` vs `"."`
- *Time*: 1 Class + 1 Tutorial  
- *Author*: A  
- *Notes*: No polymorphism yet.  
- *Assignment*: Number Theory & Logic Ops

---

== 🧮 Chapter 4: Types as Sets

- *Topics*:  
  - Tuples as $times$, `Either` as $union$, `(->)` as $A^B$, `::` as $in$
  - Currying (concept only), implicit parentheses  
  - Basic set theory concepts
- *Time*: 1 Class + 1 Tutorial  
- *Author*: R  

---

== 📃 Chapter 5: Lists

- *Topics*:  
  - List definition & comprehension  
  - Lists as syntax trees  
  - Operations: `head`, `tail`, `!!`, `elem`, `drop`, `take`, `splitAt`  
  - Merge sort, infinite lists  
  - Code Examples:
    ```haskell
    l = 0 : l
    l n = n : l (n+1)
    l a b = a : l b (a+b)
    ```
- *Time*: 2 Classes + 2 Tutorials  
- *Author*: R  

---

== 🔁 Chapter 6: Polymorphism & Higher Order Functions

- *Topics*:  
  - Intro to polymorphism  
  - Higher-order functions  
  - Operators and functions:
    ```haskell
    ($) :: (a -> b) -> a -> b
    a -> b -> (a, b)
    curry, uncurry
    ```
- *Time*: 1 Class + 2 Tutorials  
- *Author*: S  

---

== 🔃 Chapter 7: Advanced List Operations

- *Topics*:  
  - `map`, `filter`, Cartesian product, first through list comprehension, then explicitly defined
  - Quick sort through list comprehension
  - `zip`, `zipWith`  
  - Folds, scans (with syntax tree understanding)  
  - Miller–Rabin primality test
- *Time*: 2 Classes + 3 Tutorials  
- *Author*: A  

---

== 📊 Chapter 8: Precomp Data Structures
- *Topics*:
  - Define recursion in recursive data types
  - Define whatever happened in the basic datatypes section.
  - Define Nat, List, Tree
  
- *Time*:
- *Author*: S

== 🧵 Chapter 9: Computation as Reduction

- *Topics*:  
  - Reduction-based computation (skip Big O)  
  - Syntax trees, lazy evaluation  
  - Examples:
    - Fibonacci via infinite list
    - Test if the following works:
      ```haskell
      (map recip [-5..]) !! n
      ```
- *Time*: 1 Class + 2 Tutorials  
- *Author*: S  

== ☣️ Chapter 10: Complexity
- *Topics*:
  - Some Notion of complexity that is pretty theoretical
  
- *Time*:
- *Author*: A

== 📊 Chapter 11: Post Comp Data Types

- *Topics*:
  - Queue
  - Segment Trees
  - BST
  - Set
  - Map
  
- *Time*:
- *Author*: A

== 🏫 Chapter 12: Typeclasses

- *Topics*:
  - Recall Polymorphism
  - `deriving`
  - Under the hood of `deriving`
  - Custom Classes
  
- *Author*: R
- *Time*:

== ⏩ Chapter 13: Monads
- *Topics*:
  - Functors
  - Applicative Functors?
  - Monads:
    - Theory 
    - Do notation
    - Simple Writer - Cost as `(,) Integer`
    - Maybe Monad 
    - Simple Reader - `(->) x`
    - Simple State - Light Switch
    - Monoid Monad
    - Simple Writer - IO
	
- *Author*: R
- *Time*: 5 Classes + 5 Tutorials

---

= 🗂 Directory Structure
In general, do not make new typst files in the chapter area, just edit the previously existent ones.
If you use a figure, please save it as a separate .typ or .asy file in figures and import when required.

```bash
haskell-course/
├── Modules
│   ├── Book.typ
│   ├── Box.typ
│   ├── Chapter.typ
│   ├── Code.typ
│   ├── Contents.typ
│   ├── Definition.typ
│   ├── Exercise.typ
│   ├── Prelude.typ
│   ├── Proof.typ
│   ├── Tree.typ
│   ├── style.typ
│   ├── theme.tmTheme
│   └── theorems.typ
├── chapters-typ
│   ├── ch01_functions.typ
│   ├── ch02_setup_linux.typ
│   ├── ch02_setup_mac.typ
│   ├── ch02_setup_win.typ
│   ├──...
│   ├── ch13_monad.typ
│   └── example_chapter.typ
├── example.pdf
├── extra-typ
│   ├── appendix.typ
│   └── preface.typ
├── figures
├── generate.sh
├── guide.md
├── guide.pdf
├── guide.typ
├── licenses
│   ├── code_license.txt
│   └── text_license.txt
├── main.pdf
├── main.typ
└── tutorials-typ
```
Tutorials, assignments and solutions refer to tutorial handouts(if needed), class and tutorial assignments and their solution files. Keep them as .typ/.tex files for now, the required .hs/.lhs files will be generated later.

Also, if you need to cite something, cite it at the end of your chapter as a comment starting with cite.
```typ
// cite:
// citation 1
// citation 2
```
I will at some point make a script to compile citations.
OR
We can use https://typst.app/docs/reference/model/cite/ and Hayagriva

= Pedagogy

== Personality of Narrator

We always use the "we" grammatical turn as far as possible, and a few steps further still.

== How to know whether a Concept has been Learnt

$ "By testing whether they can parse Haskell in natural language" $

We can test a student upon their knowledge of a Haskell function by asking the student to narrate in detail in a natural language such as English, the steps the function is taking in the execution of its definition.

For example, 

Consider the following problem : We have to make a function that provides feedback on a quiz. We are given the marks obtained by a student in the quiz marked out of 10 total marks. If the marks obtained are less than 3, return `'F'`, otherwise return the marks as a percentage -

```haskell
-- | function to an either type
feedback :: Integer -> Either Char Integer
--                     Left ~ Char,Integer ~ Right
feedback n
  | n < 3     = Left  'F'
  | otherwise = Right ( 10 * n ) -- multiply by 10 to get percentage
```

You then ask the student to describe in detail how the function works. 

They should ideally answer -
"

Let `feedback` be a function that takes an `Integer` as input and returns `Either` a `Char` or an `Integer`. \

As `Char` and `Integer` occurs on the left and right of each other in the expression `Either Char Integer`, thus `Char` and `Integer` will henceforth be referred to as `Left` and `Right` respectively. 

Let the input to the function `feedback` be `n`.

If `n<3`, then we return `'F'`. To denote that `'F'` is a `Char`, we will tag `'F'` as `Left`. (remember that `Left` refers to  `Char`!)

`otherwise`, we will multiply `n` by `10` to get the percentage out of 100 (as the actual quiz is marked out of 10). To denote that the output `10*n` is an `Integer`, we will tag it with the word `Right`. (remember that `Right` refers to `Integer`!)

"

== What does it Mean to Teach a Haskell Concept ?

Teaching is a very abstract notion. We can make that notion more explicit by assuming that what we actually want to do is get the student to a position where they can pass the test described in the above section with flying colours, i.e., 
$ "Teaching" approx "Teaching to pass the test of the previous section" $

== How to Teach a Haskell Concept

Assuming the suppositions of the previous sections hold any water, the method to teach a Haskell concept appears quite simple - 
$ "Show the students how to parse Haskell in natural language over and over again until they get it" $


= Using Imported Functions

#let c(i:"",code_block) = [
  #box(width:100%, height: auto, fill: gradient.linear(white,black.lighten(40%)).sharp(2), radius: 2pt, inset:(x: 0pt, y:5pt), stroke: gradient.linear(yellow,black.lighten(40%)).sharp(2))[
    #align(center)[#stack(dir:ltr,spacing:0pt,
    align(center+horizon)[#box(width:50%,height:auto, inset:(x:3pt, y:1pt))[
      // #set text(size:0.8em)
      #align(center+horizon)[#box[
        #align(left+top)[#raw(lang:"typ", code_block)]
      ]]
    ]],
    align(center+horizon)[#box(width:50%, height: auto,inset:(x:3pt, y:1pt))[
      #align(center)[#box(fill: white, width: 98%,height:auto, inset: 7pt)[
        #eval("["+i+"\n"+code_block+"]")
      ]]
    ]]
    )]
  ]
]

#show raw.where(lang:"typ") : it => [#set text(size:0.8em) 
#it]

#let show_import(it) = raw(lang:"typ", it.replace("\"Modules","\"../Modules"))

== Appendix


#let i = "#import \"Modules/Appendix.typ\": appendixed"

To use this module, we need to #show_import(i)

Apply the ```typ #appendixed``` function on some ```typ content``` to move it to the ```typ Appendix``` chapter.

#c(i:i,"
You can find more information about this fancy stuff in the 
#appendixed[
  === Fancy Stuff
  Some fancy stuff
].
")

#block(height:1pt,width:1pt)[
  #hide[
    #show pagebreak : {}
    #import "Modules/Appendix.typ" : appendix
    #appendix
  ]
]

The word "appendix" on the right is a link to the location where the content has been moved to in the ```typ Appendix``` chapter.

== Code Block

We can write any text that is meant to be code using the usual syntax of using backticks (```typ ` ```).

=== Inline Code Block

We can make a code block in the middle of a line.
#c("Let us take the `hello` function as an example.")

=== Floating Code Block

We can make a code block that floats out-of-line on its own.
#c("```
hello :: any -> String
hello     _  =  \"world\"
```")

=== Literate Haskell

We are using ```cabal markdown-unlit``` as our literate haskell pre-processor. It only processes those *_floating_* code blocks at *_zero indentation depth_* which have been *_marked as haskell_* code blocks. So, if you want your code to be visible to lhs, you need to write "haskell" immediately after the 3 backticks, leaving no space in between.
#c("```haskell
hello :: any -> String
hello     _  =  \"world\"
```")

=== Hiding a Code Block

We can hide a code block by putting it inside the Typst function ```typ #metadata()```.

#c("#metadata[
```
hello :: any -> String
hello     _  =  \"world\"
```
]")

This can be *_useful_* if paired with the syntax required for *_Literate Haskell_*, as then we can have code that doesn't appear on the PDF, but still executable by Literate Haskell. \ And thus readers would still be able to access it in GHCi without it taking up valuable reading space on the PDF.

=== Code Block Title

We will often find it a good idea to title a code block, because then it will show up in the table of contents, in the glossary and can be referenced. \ 
If the first line of the code in a _*floating*_ code block begins with ```-- | ```, then the rest of that line will be taken as the title.

// hidden from contents
#c("```
-- | helloWorld function
hello :: any -> String
hello     _  =  \"world\"
```")

=== Referencing a Code Block

A *_titled_* definition with a *_unique title_* can referenced by the usual syntax.

#c("Recalling @code_of_helloWorld_function, we can proceed.")

== Definition 

#let i = "#import \"Modules/Definition.typ\": def"

To use this module, we need to #show_import(i)

=== Definition Box

We can call the function ```typ #def()``` upon some ```typ content``` put that ```typ content``` in a _*floating*_ definition box. \
We can put any text that is meant to be definition in a definition box.

#c(i:i,"#def[
  The empty set is the set that contains no elements or equivalently, ${}$.
]")

=== Emphasizing the Subject

To increase readability, we can emphasize the subject of the dinition by wrapping it in ```typ **```.

#c(i:i,"#def[
  The *empty set* is the set that contains no elements or equivalently, ${}$.
]")

=== Emphasizing the Definition

To increase readability, we can emphasize _*the part of the text that is the actual definition*_ by wrapping that part in ```typ __```.

#c(i:i,"#def[
  The empty set is the _set that contains no elements_ or equivalently, _${}$_.
]")

=== Definition Box Title

We will often find it a good idea to title a definition, because then it will show up in the table of
contents, in the glossary and can be referenced. \
We can set the ```typ sub``` settable argument of the ```typ #def()``` function to a _*```typ string```*_ if we want to add a title.

#c(i:i,"#def(sub: \"empty set\")[
  The empty set is the set that contains no elements or equivalently, ${}$.
]")

=== Referencing a Definition

A *_titled_* definition with a *_unique title_* can referenced by the usual syntax.

#c("Recalling @definition_of_empty_set, we can proceed.")


== Exercise

#let i = "#import \"Modules/Exercise.typ\": exercise" 

To use this module, we need to #show_import(i)

// #c(i:i,"#exercise(sub: \"maybe\" )[
//   If a type `T` has $n$ elements, then how many elements does `Maybe T` have?
// ]")

=== Exercise Box

We can call the function ```typ #exercise()``` upon some ```typ content``` put that ```typ content``` in a _*floating*_ exercise box. \ 
We can put any text that is meant to be an exercise in an exercise box.

#c(i:i,"#exercise[
  If a type `T` has $n$ elements, then how many elements does `Maybe T` have?
]")

=== Exercise Box Custom Title

We will often find it a good idea to title an exercise, because then it will show up in the collection of exercises when we use ```typ #exercises```, in the glossary and can be referenced. \
We can set the ```typ sub``` settable argument of the ```typ #exercise()``` function to a _*```typ string```*_ if we want to add a title.

#c(i:i,"#exercise(sub: \"maybe\" )[
  If a type `T` has $n$ elements, then how many elements does `Maybe T` have?
]")

=== Referencing an Exercise

A *_titled_* exercise with a *_unique title_* can referenced by the usual syntax.

#c("Recalling @exercise_of_maybe, we can proceed.")

== Proof

#let i = "#import \"Modules/Proof.typ\": proof" 

To use this module, we need to #show_import(i)

=== Proof Environment

We can call the function ```typ #proof()``` upon some ```typ content``` to prepend that ```typ content``` with a "Proof" tag.  \ 
We can treat any text that is meant to be an proof in this manner.

#c(i:i,"#proof[
  Here is a proof.
]")

=== Theorem Environment

To add line with a "Theroem" tag above the proof, we can set the ```typ thm``` settable argument of the ```typ #proof``` function to that line.

#c(i:i,"#proof( thm: [This is a theorem statement.] )[
  Here is a proof.
]")


== Quote 

#let i = "#import \"Modules/Quote.typ\": quote" 

To use this module, we need to #show_import(i)

=== Quote Box

We can call the function ```typ #quote()``` upon some ```typ content``` put that ```typ content``` in a _*floating*_ quote box. \ 
Quote boxes are meant to have quotes from other source materials like articles or books or people.

#c(i:i,"#quote[
  The name is Bond, James Bond.
]")

=== Quote Box Title

Title will be used to give context to the quote, where they are from and so on, and one can reference a quote using this. \
We can set the ```typ sub``` settable argument of the ```typ #quote()``` function to a _*```typ string```*_ if we want to add a title.

#c(i:i,"#quote(sub: \"Daniel Craig, Casion Royale\" )[
  The name is Bond, James Bond.
]")

=== Referencing a Quote

This I need to figure out, i don't know if commas can be a part of the name.

== Tree

#let i = "#import \"Modules/Tree.typ\": tree" 

To use this module, we need to #show_import(i)

=== Conversion

The following function "convert" converts a tree whose nodes are Typst ```typ content``` into data that Typst can interpret as a tree. 

#import "Modules/Tree.typ": tree, far_away, dots
convert : Tree TypstContent $->$ TypstTreeData \
convert(```typ node```) $:=$ ```typ node``` \ 
convert(#tree(spread: 2.5,(```typ parent_node```,```typ sub_tree_1```, ```typ sub_tree_2```,dots, ```typ sub_tree_n```))) \ \ $"  ":=$ ```typ (parent_node,convert(sub_tree_1),convert(subtree_2),...,convert(sub_tree_n))```

For example,\ $"           "$ #tree(([A],([B],[D],[E]),([C],[F],[G]))) converts to ```typ ([A],([B],[D],[E]),([C],[F],[G]))```

=== Displaying a Tree

Once you've converted your tree, you can display it by applying the typst function ```typ #tree()``` on the data obtained upon conversion, i.e, ```typ #tree()``` : TypstTreeData $->$ TypstTreeDisplay. 

#c(i:i,"#tree(([A],([B],[D],[E]),([C],[F],[G])))")

=== Padding

We can control how much whitespace padding surrounds each node by setting the ```typ pad``` coefficient settable argument of the ```typ #tree()``` function, \ usually to ensure that the edge does not touch the content of the node.

#c(i:i,"#tree(pad: 0.55,([A],([B],[D],[E]),([C],[F],[G])))")

=== Width of a Tree

We can control the width of a tree by the ```typ spread``` coefficient settable argument of the ```typ #tree()``` function.

#c(i:i,"#tree(spread: 2,([A],([B],[D],[E]),([C],[F],[G])))")

=== Depth of a Tree

We can control the depth of a tree by the ```typ grow``` coefficient settable argument of the ```typ #tree()``` function.

#c(i:i,"#tree(grow: 2,([A],([B],[D],[E]),([C],[F],[G])))")

=== Wiiiide Trees

#let j = "#import \"Modules/Tree.typ\": dots"

To do this we need to #show_import(j)

We can suggest that a tree is very wide by making one the nodes the ```typ #dots``` function from this module.

#c(i:i+"\n"+j,"#tree(
  ($f $,
    $x_1$,
    $x_2$,
    $x_3$,
    dots,
    $x_(n-1)$,
    $x_n$
  )
) ")

=== Deeeeep Trees

#let j = "#import \"Modules/Tree.typ\": far_away"

To do this we need to #show_import(j)

We can suggest that a tree is very wide by applying ```typ #far_away()``` function from this module on one of the nodes.

#c(i:i+"\n"+j,"#tree(
  (`(:)`,
    `x₁`,
    (`(:)`,
      `x₂`,
      (`(:)`,
        `x₃`,
        (far_away(`(:)`),
          `xₙ₋₁`,
          (`(:)`,
            `xₙ`,
            `[]`
          )
        )
      )
    )
  )
)")

]

#{
  
import "Modules/Book.typ"
import "Modules/Box.typ"
import "Modules/Chapter.typ"
import "Modules/Code.typ"
import "Modules/Contents.typ"
import "Modules/Prelude.typ"

let title = [Authors' Guide]
let author = [Ryan Hota, Shubh Sharma, Arjun Maneesh Agarwal]

Book.settings(title:title,author:author,{
  set page( height: auto)
Box.settings({
Code.settings({
Contents.settings({
Prelude.settings({

Book.title_page(title:title,author:author)

set page(numbering: "i") ; counter(page).update(1)

Book.copyright_declaration(title:title,author:author)

Book.dedication([To a job well done])

Contents.contents

set page(numbering: "1") ; counter(page).update(1)

set heading( offset : 1 )

body

})
})
})
})
})

}
