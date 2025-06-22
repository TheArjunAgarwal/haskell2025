#import "Modules/Book.typ"
#import "Modules/Box.typ"
#import "Modules/Chapter.typ"
#import "Modules/Code.typ"
#import "Modules/Contents.typ"
#import "Modules/Prelude.typ"

#show: Book.settings
#show: Box.settings
#show: Code.settings

#import "@preview/touying:0.6.1": *
#import themes.simple: *

#show: simple-theme.with(aspect-ratio: "16-9")
#import "Modules/Definition.typ" : def
#import "Modules/Proof.typ" : proof
#import "Modules/Exercise.typ" : exercise
#import "Modules/Tree.typ" : tree, dots
#let d = sym.colon.eq
#let e = $==$

#title-slide[Basic Theory]

== Mathematics vs Haskell //todo (taking suggestions for a better heading)

The main difference between mathematics and haskell is *who* reads what we write.

#pause
When writing any form of mathematical expression, it is the expectation that it is meant to be read by humans, and convince them of some mathematical proposition.\
#pause
On the other hand, haskell code is not _primarily_ meant to be read by humans, but rather by machines. The computer reads haskell code, and tries to interpret it into steps of manipulating expressions.

---

When writing mathematics, we can choose to be a bit sloppy and hand-wavy with our words, as we can rely to some degree on the imagination and pattern-sensing abilities of the reader to fill in the gaps.

However, in this context, computers, being unintelligent machines, are extremely dumb and stupid. Unless we spell out the details for them in excruciating detail, they are not going to understand what we want them to do.

---

Since in this course we are going to be writing for computers, we need to ensure that our writing is very precise, correct and generally *idiot-proof*. (Because, in short, computers are idiots)

#pause
In order to practice this more formal style of writing required for *haskell code*, the first step we can take is to know how to write our familiar *mathematics* more formally.

== The Building Blocks

#box[
#set text(size:0.9em)
The language of writing mathematics is fundamentally based on two things -

- *Symbols:* such as $0,1,2,3,x,y,z,n,alpha,gamma,delta,NN,QQ,RR,in,<,>,f,g,h,=>,forall,exists$ etc.
and

- *Expressions:* which are sentences or phrases made by chaining together these symbols, such as 
  - $x^3 dot x^5 + x^2 + 1$
  - $f(g(x,y),f(a,h(v),c),h(h(h(n))))$
  - $forall alpha in RR " " exists L in RR " " forall epsilon > 0 " " exists delta > 0 " " | x - alpha | < delta => | f(x) - f (alpha) | < epsilon$
  #v(5pt,weak:true) etc.
]

---

#import "Modules/Appendix.typ" : appendixed

There are expressions like $x y x<== forall => f (  <~> arrow(v)$, \ which are meaningless, \
and there are expressions like $x^3 dot x^5 + x^2 + 1$,\ which are meaningful.

These meaningful expressions are called @definition_of_well-formed_mathematical_expression s. \ There is a formal definition in the 
#appendixed[
= Values

#def(sub : "mathematical value")[
  A mathematical *value* is a single and specific well-defined mathematical object that is constant, i.e., does not change from scenario to scenario nor represents an arbitrary object.
]

Examples include - 
- The real number $pi$
// - The set of rational numbers $QQ$
- The order $<$ on $NN$
- The function of squaring a real number $: RR -> RR$
- The number of non-trivial zeroes of the Riemann Zeta function

Therefore we can see that relations and functions can also be *values*, as long as they are constant, specific, and not scenario-dependent.

In fact, as we see in the last example, even if we don't know what the exact value is, we can still know that it is *some value*,\ as it is a constant, even though it is an unknown constant.

= Variables

#def(sub : "mathematical variable")[
  A mathematical *variable* is a symbol or chain of symbols meant to represent a value that is arbitrary in some way, usually as a way to show that whatever process follows can be carried out with any arbitrary value.
]

For example, consider the following theorem - \
#proof(thm:[Adding $1$ to a natural number makes it bigger.])[
  Take $n$ to be an arbitrary natural number.\
  We know that $1 > 0$.\
  Adding $n$ to both sides of the preceding inequality yields $ n + 1 > n $
]
Here, $n$ is a variable as it isn't any specific value, but rather an arbitrary instance of a certain type of value. \
It has been used to show a certain fact that holds for *any* natural number.

= Well-Formed Expressions

Consider the expression - $ x y x<== forall => f (  <~> arrow(v)  $
It is an expression as it *is* a bunch of symbols arranged one after the other, but the expression is obviously meaningless.\

So what distinguishes a meaningless expression from a meaningful one? Wouldn't it be nice to have a systematic way to check  whether an expression is meaningful or not?\

Indeed, that is what the following definition tries to achieve - a systematic method to detect whether an expression is well-structured enough to possibly convey any meaning.

#def(sub:"well-formed mathematical expression")[
  It is difficult to give a direct definition of a *well-formed expression*. As an alternative, we can define a _formal procedure_ to check whether an expression is  well-formed or not.

  The procedure is as follows - 

  Given an expression _$e$_, 
  
    - first check whether _$e$_ is a 
      - @definition_of_mathematical_value 
      #v(0pt,weak:true) or #v(5pt,weak:true)
      - @definition_of_mathematical_variable
      #v(5pt,weak:true) in which cases _$e$_ passes the check and is an expression.
    
    Failing that,
  
    - check whether _$e$_ is of the form _$f(e_1,e_2,e_3,...,e_n)$_, where 
      - _$f$_ is a function #text(size:0.8em)[(the function can be a @definition_of_mathematical_value or @definition_of_mathematical_variable)]
      - which takes _$n$_ inputs, 
      #v(5pt,weak:true) and #v(5pt,weak:true)
      - _$e_1,e_2,e_3,...,e_n$_ are all _well-formed expressions_ which are _valid inputs_ to _$f$_.
]


Let us use this defining procedure to check if $x^3 dot x^5 + x^2 + 1$ is a well-formed expression. \
( We will skip the check of whether something is a valid input or not, as that notion is still not very well-defined for us. )

$x^3 dot x^5 + x^2 + 1$ is $+$ applied to the inputs $x^3 dot x^5$ and $x^2 + 1$. \
Thus we need to check that $x^3 dot x^5$ and $x^2 + 1$ are well-formed expressions which are valid inputs to $+$.

$x^3 dot x^5$ is $dot$ applied to the inputs $x^3$ and $x^5$. \
Thus we need to check that $x^3$ and $x^5$ are well-formed expressions.

$x^3$ is $(" ")^3$ applied to the input $x$. \
Thus we need to check that $x$ is a well-formed expression.

$x$ is a well-formed expression, as it is a @definition_of_mathematical_variable.

$x^5$ is $(" ")^5$ applied to the input $x$. \
Thus we need to check that $x$ is a well-formed expression.

$x$ is a well-formed expression, as it is a @definition_of_mathematical_variable.

$x^2 + 1$ is $+$ applied to the inputs $x^2$ and $1$. \
Thus we need to check that $x^2$ and $1$ are well-formed expressions.

$x^2$ is $(" ")^2$ applied to the input $x$. \
Thus we need to check that $x$ is a well-formed expression.

$x$ is a well-formed expression, as it is a @definition_of_mathematical_variable.

$1$ is a well-formed expression, as it is a @definition_of_mathematical_value.

Done!

#exercise(sub:"checking well-formedness of mathematical expression")[
  Check whether $f(g(x,y),f(a,h(v),c),h(h(h(n))))$ is a well-formed expression or not.
]
]

== Defining Functions

Functions are a very important tool in mathematics and they form the foundations of Haskell programming. \ 

#pause
Thus, it is very helpful to have a deeper understanding of how they are defined.

== Using Expressions

In its simplest form, a definition of a function is made up of a left-hand side, '$#d$' in the middle#footnote[
  In order to have a clear distinction between notation and equality,\
  we use $A #d B$ to mean "$A$ is defined to be $B$",\
  and we use $A #e B$ to mean "$A$ is equal to $B$".
], and a right-hand side.

#alternatives[
On the left we write the name of the function followed by a number of variables which represent its inputs.
][
In the middle we write '$#d$', indicating that right-hand side is the definition of the left-hand side.
][
On the right, we write a @definition_of_well-formed_mathematical_expression using the variables of the left-hand side, describing to how to combine and manipulate the inputs to form the output of the function.
]

---

A few examples -
  - $f(x) #d x^3 dot x^5 + x^2 + 1$
  - more examples //todo


== Some Conveniences

Often in the complicated definitions of some functions, the right-hand side expression can get very convoluted, so there are some conveniences which we can use to reduce this mess.

== Where, Let //todo

// ordinary function definition

which, for convenience, can be rewritten as - 

// definition rewritten using where

or as -

// definition rewritten using let

== Anonymous Functions //todo

// ordinary function definition

which, for convenience, can be rewritten as - 

// definition rewritten as (arg |-> expr)

which is particularly useful when we (for some reason) do not want name the function.

This notation can also be used when there are multiple inputs.

Consider -

// ordinary function definition

which, for convenience, can be rewritten as - 

// definition rewritten as (x,y,z |-> expr)

== Piecewise Functions //todo 

== Pattern Matching //todo

//intro as an easier alternative to pattern matching

// ordinary function definition

which, for convenience, can be rewritten as - 

// definition rewritten as pattern matching, ensure that there are overlapping patterns

//explain how to deal with overlapping patterns

== Recursion

A function definition is recursive when the name of the function being defined appears on the right-hand side as well.

#pause
For example, consider defining the famous fibonacci function - 

$
  F &: NN -> NN \
  F(0) &#d 1 \
  F(1) &#d 1 \
  F(n) &#d F(n-1)+F(n-2)
$

== Termination

But it might happen that a recursive definition might not give a final output for a certain input.

#pause
For example, consider the following definition - $ f(n) &#d f(n+1) $

#pause
It is obvious that this definition does not define an actual output for, say, $f(4)$.

---

However, the previous definition of $F$ obviously defines a specific output for $F(4)$ as follows -
#[
#set text(size:0.8em)
$
  F(4) #pause&#e F(3) + F(2)\
  #pause&#e ( F(2) + F(1) ) + F(2)\
  #pause&#e ( ( F(1) + F(0) ) + F(1) ) + F(2)\
  #pause&#e ( ( 1 + F(0) ) + F(1) ) + F(2)\
  #pause&#e ( ( 1 + 1 ) + F(1) ) + F(2)\
  #pause&#e ( 2 + F(1) ) + F(2)\
  #pause&#e ( 2 + 1 ) + F(2)\

$
]

---

$
  F(4) &#e ( 2 + 1 ) + F(2)\
  #pause&#e 3 + F(2)\
  #pause&#e 3 + ( F(1) + F(0) )\
  #pause&#e 3 + ( 1 + F(0) )\
  #pause&#e 3 + ( 1 + 1 )\
  #pause&#e 3 + 2\
  #pause&#e 5 
$

----

#def(sub:"termination of recursive definition")[
  In general, a recursive definition is said to *terminate on an input* \ $<=>$ it eventually gives an _actual specific output for that input_.
]

#pause
But what we cannot do this for every $F(n)$ one by one.

#pause
What we can do instead, is use a powerful tool known as the @definition_of_principle_of_mathematical_induction.

== Induction

#def(sub:"principle of mathematical induction")[
  If we have an infinite sequence of statements $phi_0,phi_1, phi_2, phi_3, . . . $\
  and we can prove the following 2 statements -
  - $phi_0$
  - $forall n ( phi_n => phi_(n+1) )$
  then all the statements $phi_0,phi_1, phi_2, phi_3, . . . $ in the sequence are true.
]

== Proving Termination using Induction

So let's see the @definition_of_principle_of_mathematical_induction in action, and use it to prove that 
#proof(thm:[The definition of the fibonacci function $F$ terminates for any natural number $n$.])[]
---
#pause
  For each natural number $n$, let $phi_n$ be the statement 
  #align(center)[" The definition of $F$ terminates for every natural number which is $<= n" "$"]
#pause
  To apply the @definition_of_principle_of_mathematical_induction, we need only prove the 2 requirements and we'll be done. So let's do that -
  
---

#alternatives[
  - *$angle.l angle.l" "phi_0" "angle.r angle.r $* \ 
    The only natural number which is $<=0$ is $0$, and $F(0) #d 1$, so the definition terminates immediately.
  
][
  - *$angle.l angle.l" "forall n ( phi_n => phi_(n+1) )" "angle.r angle.r $* \ 
    Assume that $phi_n$ is true. \
    Let $m$ be an arbitrary natural number which is $<= n+1.$ 

]

---

    - $angle.l angle.l" "$ Case 1 $" "(m<=1)$ $" "angle.r angle.r$ \ \
      $F(m) #d 1$, so the definition terminates immediately.
---

    - $angle.l angle.l" "$ Case 2 $" "(m>1)$ $" "angle.r angle.r$ \ \
#pause
      $F(m) #d F(m-1) + F(m-2)$, \ \
#pause
      and since $m-1$ and $m-2$ are both $<= n$, \ \
#pause
      $phi_n$ tells us that both $F(m-1)$ and $F(m-2)$ must terminate. \ \
#pause
      Thus $F(m) := F(m-1) + F(m-2)$ must also terminate. 

---

    Hence $phi_(n+1)$ is proved!

---

#box(height:0pt,width:0pt,clip:true)[
#import "Modules/Appendix.typ" : appendix
#appendix
]