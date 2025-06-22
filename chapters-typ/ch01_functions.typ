#import "../Modules/Definition.typ" : def
#import "../Modules/Proof.typ" : proof
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Tree.typ" : tree, dots
#let d = sym.colon.eq
#let e = $==$

= Mathematics vs Haskell //todo (taking suggestions for a better heading)

The main difference between mathematics and haskell is *who* reads what we write.

When writing any form of mathematical expression, it is the expectation that it is meant to be read by humans, and convince them of some mathematical proposition.\
On the other hand, haskell code is not _primarily_ meant to be read by humans, but rather by machines. The computer reads haskell code, and tries to interpret it into steps of manipulating expressions.

When writing mathematics, we can choose to be a bit sloppy and hand-wavy with our words, as we can rely to some degree on the imagination and pattern-sensing abilities of the reader to fill in the gaps.

However, in this context, computers, being unintelligent machines, are extremely dumb and stupid. Unless we spell out the details for them in excruciating detail, they are not going to understand what we want them to do.

Since in this course we are going to be writing for computers, we need to ensure that our writing is very precise, correct and generally *idiot-proof*. (Because, in short, computers are idiots)

In order to practice this more formal style of writing required for *haskell code*, the first step we can take is to know how to write our familiar *mathematics* more formally.

= The Building Blocks

The language of writing mathematics is fundamentally based on two things -
- *Symbols:* such as $0,1,2,3,x,y,z,n,alpha,gamma,delta,NN,QQ,RR,in,<,>,f,g,h,=>,forall,exists$ etc.
and
- *Expressions:* which are sentences or phrases made by chaining together these symbols, such as 
  - $x^3 dot x^5 + x^2 + 1$
  - $f(g(x,y),f(a,h(v),c),h(h(h(n))))$
  - $forall alpha in RR " " exists L in RR " " forall epsilon > 0 " " exists delta > 0 " " | x - alpha | < delta => | f(x) - f (alpha) | < epsilon$
  #v(5pt,weak:true) etc.

= Values

#def(sub : "mathematical value")[
  A mathematical *value* is a single and specific well-defined mathematical object that is constant, i.e., does not change from scenario to scenario nor represents an arbitrary object.

  The following examples should clarify further.
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
  A mathematical *variable* is a symbol or chain of symbols meant to represent a @definition_of_mathematical_value that is arbitrary in some way, usually as a way to show that whatever process follows can be carried out with any arbitrary value.

  The following example should clarify further.
]

For example, consider the following theorem - \
#proof(thm:[Adding $1$ to a natural number makes it bigger.])[
  Take $n$ to be an arbitrary natural number.\
  We know that $1 > 0$.\
  Adding $n$ to both sides of the preceding inequality yields $ n + 1 > n $
  Hence Proved !!
]
Here, $n$ is a variable as it isn't any specific value, but rather an arbitrary instance of a certain type of value. \
It has been used to show a certain fact that holds for *any* natural number.

= Well-Formed Expressions

Consider the expression - $ x y x<== forall => f (  <~> arrow(v)  $
It is an expression as it *is* a bunch of symbols arranged one after the other, but the expression is obviously meaningless.\

So what distinguishes a meaningless expression from a meaningful one? Wouldn't it be nice to have a systematic way to check  whether an expression is meaningful or not?\

Indeed, that is what the following definition tries to achieve - a systematic method to detect whether an expression is well-structured enough to possibly convey any meaning.

#def(sub:"well-formed mathematical expression")[
  We can define a _formal procedure_ to check whether an expression is  well-formed or not.

  The procedure is as follows - 

  Given an expression _$e$_, 
  
    - first check whether _$e$_ is a 
      - @definition_of_mathematical_value 
      #v(0pt,weak:true) or #v(5pt,weak:true)
      - @definition_of_mathematical_variable
      #v(5pt,weak:true) in which cases _$e$_ passes the check and is an expression.
    
    Failing that,
  
    - check whether _$e$_ is of the form _$f(e_1,e_2,e_3,...,e_n)$_, where 
      - _$f$_ is a function
      - which takes _$n$_ inputs, 
      #v(5pt,weak:true) and #v(5pt,weak:true)
      - _$e_1,e_2,e_3,...,e_n$_ are all _well-formed expressions_ which are _valid inputs_ to _$f$_.
]

*Remark:* (the function $f$ can be a @definition_of_mathematical_value or @definition_of_mathematical_variable)

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

= Defining Functions

Functions are a very important tool in mathematics and they form the foundations of Haskell programming. \ 
Thus, it is very helpful to have a deeper understanding of how they are defined.

== Using Expressions

In its simplest form, a definition of a function is made up of a left-hand side, '$#d$' in the middle#footnote[
  In order to have a clear distinction between notation and equality,\
  we use $A #d B$ to mean "$A$ is defined to be $B$",\
  and we use $A #e B$ to mean "$A$ is equal to $B$".
], and a right-hand side.

On the left we write the name of the function followed by a number of variables which represent its inputs.

In the middle we write '$#d$', indicating that right-hand side is the definition of the left-hand side.

On the right, we write a @definition_of_well-formed_mathematical_expression using the variables of the left-hand side, describing to how to combine and manipulate the inputs to form the output of the function.

A few examples -
  - $f(x) #d x^3 dot x^5 + x^2 + 1$
  - $"second"(a,b) #d (a,b)$
  - $zeta(s) #d Sigma^infinity_(n=1) 1/(n^s)$

== Some Conveniences

Often in the complicated definitions of some functions, the right-hand side expression can get very convoluted, so there are some conveniences which we can use to reduce this mess.

=== Where, Let

Consider the definition of the famous $"sine"$ function - \ 
\
$ "sine" : RR -> RR $
Given an angle $theta$,\
let $T$ be a right-angled triangle, one of whose angles is $theta$.\
Let $p$ be the length of the perpendicular of $T$.\
Let $h$ be the length of the hypotenuse of $T$.\
Then $ "sine"(theta) #d p/h $

Here we use the variables $p$ and $h$ in the right-hand side of the definition, but to get their meanings one will have to look at how they are defined beforehand in the lines beginning with "let".

We can also do this using "where" instead of "let".

$ "sine" : RR &->  RR \
  "sine"(theta) &#d p/h\
  "     ,where"\
  "        "T &#d "a right-angled triangle with one angle == "theta\
  p &#d "the length of the perpendicular of "T\
  h &#d "the length of the hypotenuse    of "T
$

Here we use the variables $p$ and $h$ in the right-hand side of the definition, but to get their meanings one will have to look at how they are defined after "where".

=== Anonymous Functions

A function definition such as

$
  f : RR &->  RR\
  f(x) &#d x^3 dot x^5 + x^2 + 1
$

which, for convenience, can be rewritten as - 

$
  ( x |-> x^3 dot x^5 + x^2 + 1 ) : RR -> RR
$

Notice that we did not use the symbol $f$, which is the name of the function, which is why this style of definition is called "anonymous".

Also, we used $|->$ in place of $:=$

This style is particularly useful when we (for some reason) do not want name the function.

This notation can also be used when there are multiple inputs.

Consider -

$
  "harmonicSum" : RR_(>0) times RR_(>0) &-> RR_(>0)\
  "harmonicSum"(x,y) &#d 1/x + 1/y
$

which, for convenience, can be rewritten as - 

$
  ( x , y |-> 1/x + 1/y ) : RR_(>0) times RR_(>0) &-> RR_(>0)
$

=== Piecewise Functions 

Sometimes, the expression on the right-hand side of the definition needs to depend upon some condition, and we denote that in the following way -

$
  "< functionName > "(x) #d cases(
    "< expression"_1" >" &"; if < condition"_1" >","",
    "< expression"_2" >" &"; if < condition"_2" >","",
    "< expression"_3" >" &"; if < condition"_3" >","",
    .,.,.,"",
    "< expression"_n" >" &"; if < condition"_n" >",
  )
$

For example, consider the following definition - 

$
  "signum"(x) #d cases( 
    +1 &"; if "x " "> &&0 , "",
    "  "0 &"; if "x == &&0 , "" ,
    -1 &"; if "x " "< &&0
    )
$

The "$"signum"$" of a real number tells the "sign" of the real number ; whether the number is positive, zero, or negative.

=== Pattern Matching

Pattern Matching is another way to write piecewise definitions which can work in certain situations.

For example, consider the last definition - 

$
  "signum"(x) #d cases( 
    +1 &"; if "x " "> &&0 , "",
    "  "0 &"; if "x == &&0 , "" ,
    -1 &"; if "x " "< &&0
    )
$

which can be rewritten as - 

$
  "signum"(0) &#d 0\
  "signum"(x) &#d x/(|x|)
$

This definition relies on checking the form of the input.

If the input is of the form "$0$", then the output is defined to be $0$.\
For any other number $x$, the output is defined to be $x/(|x|)$

However, there might remain some confusion -\
If the input is "$0$", then why can't we take $x$ to be $0$, and apply the second line #text(fill:blue)[($"signum"(x) &#d x/(|x|)$)] of the definition ?

To avoid this confusion, we adopt the following convention -\
Given any input, we start reading from the topmost line to the bottom-most, and we apply the first applicable definition.

So here, the first line #text(fill:blue)[($"signum"(0) &#d 0$)] will be used as the definition when the input is $0$.

== Recursion

A function definition is recursive when the name of the function being defined appears on the right-hand side as well.

For example, consider defining the famous fibonacci function - 

$
  F : NN &-> NN \
  F(0) &#d 1 \
  F(1) &#d 1 \
  F(n) &#d F(n-1)+F(n-2)
$

=== Termination

But it might happen that a recursive definition might not give a final output for a certain input.

For example, consider the following definition - $ f(n) &#d f(n+1) $

It is obvious that this definition does not define an actual output for, say, $f(4)$.

However, the previous definition of $F$ obviously defines a specific output for $F(4)$ as follows -
$
  F(4) &#e F(3) + F(2)\
  &#e ( F(2) + F(1) ) + F(2)\
  &#e ( ( F(1) + F(0) ) + F(1) ) + F(2)\
  &#e ( ( 1 + F(0) ) + F(1) ) + F(2)\
  &#e ( ( 1 + 1 ) + F(1) ) + F(2)\
  &#e ( 2 + F(1) ) + F(2)\
  &#e ( 2 + 1 ) + F(2)\
  &#e 3 + F(2)\
  &#e 3 + ( F(1) + F(0) )\
  &#e 3 + ( 1 + F(0) )\
  &#e 3 + ( 1 + 1 )\
  &#e 3 + 2\
  &#e 5 
$

#def(sub:"termination of recursive definition")[
  In general, a recursive definition is said to *terminate on an input* \ _if and only if_ \ it eventually gives an _actual specific output for that input_.
]

But what we cannot do this for every $F(n)$ one by one.

What we can do instead, is use a powerful tool known as the @definition_of_principle_of_mathematical_induction.

=== Induction

#def(sub:"principle of mathematical induction")[
  If we have an infinite sequence of statements $phi_0,phi_1, phi_2, phi_3, . . . $\
  and we can prove the following 2 statements -
  - $phi_0$
  - $"For each "n", if "phi_n" is true, then "phi_(n+1)" is also true."$
  then all the statements $phi_0,phi_1, phi_2, phi_3, . . . $ in the sequence are true.
]

=== Proving Termination using Induction

So let's see the @definition_of_principle_of_mathematical_induction in action, and use it to prove that 
#proof(thm:[The definition of the fibonacci function $F$ terminates for any natural number $n$.])[
  For each natural number $n$, let $phi_n$ be the statement 
  #align(center)[" The definition of $F$ terminates for every natural number which is $<= n" "$"]
  To apply the @definition_of_principle_of_mathematical_induction, we need only prove the 2 requirements and we'll be done. So let's do that -
  
  - *$angle.l angle.l" "phi_0" "angle.r angle.r $* \ 
    The only natural number which is $<=0$ is $0$, and $F(0) #d 1$, so the definition terminates immediately.
  
  - *$angle.l angle.l" For each "n", if "phi_n" is true, then "phi_(n+1)" is also true. "angle.r angle.r $* \ 
    Assume that $phi_n$ is true. \
    Let $m$ be an arbitrary natural number which is $<= n+1.$ \ 

    - $angle.l angle.l" "$ Case 1 $" "(m<=1)$ $" "angle.r angle.r$ \
      $F(m) #d 1$, so the definition terminates immediately.

    - $angle.l angle.l" "$ Case 2 $" "(m>1)$ $" "angle.r angle.r$ \
      $F(m) #d F(m-1) + F(m-2)$, \
      and since $m-1$ and $m-2$ are both $<= n$, \
      $phi_n$ tells us that both $F(m-1)$ and $F(m-2)$ must terminate. \
      Thus $F(m) := F(m-1) + F(m-2)$ must also terminate. 
    
    Hence $phi_(n+1)$ is proved!
  Hence the theorem is proved!!
]

= Trees

Trees are a way to structure a collection of objects. 

Trees are a fundamental way to understand expressions and how haskell deals with them.

*In fact,any object in Haskell is internally modelled as a tree-like structure.*

== Examples of Trees

Here we have a tree which defines a structure on a collection of natural numbers - 

#tree(($12$,$23$,($10$,$14$,$78$)))

The line segments are what defines the structure.

The following tree defines a structure on a collection of words from the English language - 

#tree(spread:2,([Hello],([my],[name],[is]),([Lorenzo],([Von],[Matterhorn])),([It],([is],[good],[to]),[see],[you])))

== Making Larger Trees from Smaller Trees

If we have an object - $ 89 $
and a few trees -\
$
  #tree(($12$,$23$,($10$,$14$,$78$))) "  ","  "
  #tree(($36$,($71$,$44$,$13$),$42$,($34$,$7$))) "    ","  "
  #tree(($1$,($2$,$3$,$4$,$5$),($6$,$7$,$8$)))
$
we can put them together into one large tree by connecting them with line segments, like so - 
$
  #tree(($89$,($12$,$23$,($10$,$14$,$78$)),($36$,($71$,$44$,$13$),$42$,($34$,$7$)),($1$,($2$,$3$,$4$,$5$),($6$,$7$,$8$))))
$

== Formal Definition of Trees

We will adopt a similar approach to defining trees as we did with expressions, i.e., we will provide a formal procedure to check whether a mathematical object is a tree, rather than directly defining what a tree is.

#def(sub:"tree")[
  A *tree over a set $S$* defines a meaningful structure on a collection of elements from $S$.

  The procedure to determine whether an object is a *tree over a set $S$* is as follows -

  Given a mathematical object _$t$_,

  - first check whether _$t in S$_, in which case _$t$_ passes the check, and is a *tree over $S$*

  Failing that,

  - check whether _$t$_ is of the form _#tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))_, where

    - _$p in S$_
    - and each of _$t_1, t_2, t_3, ..., t_(n-1),"and" t_n$_ is a *tree over $S$*.

]

Let us use this definition to check whether $ #tree(($12$,$23$,($10$,$14$,$78$))) $ is a *tree over the natural numbers*.\ \ 

Let's start - \ \ 

#tree(($12$,$23$,($10$,$14$,$78$))) is of the form #tree(($p$,$t_1$,$t_2$)), where $p$ is $12$ , $" "t_1$ is $23$ , and $t_2$ is #tree(($10$,$14$,$78$)).

#v(1.8em, weak:true)

Of course, $12 in NN$ and therefore $p in S$.

So we are only left to check that $23$ and #tree(($10$,$14$,$78$)) are trees over the natural numbers.\ \ 
$23 in NN$, so $23$ is a tree over $NN$ by the first check.\ \

#tree(($10$,$14$,$78$)) is of the form #tree(($p$,$t_1$,$t_2$)), where $p$ is $10$ , $" "t_1$ is $14$, and $t_2$ is $78$

Now, obviously $10 in NN$, so $p in S$.\
Also, $14 in NN$ and $78 in NN$, so both pass by the first check.

\
== Structural Induction

In order to prove things about trees, we have a version of the @definition_of_principle_of_mathematical_induction for trees - 

#def(sub:"structural induction for trees")[
  If for each tree $t$ over a set $S$, we have a statement $phi_t$,\
 
  and we can prove the following two statements  -

  - For each $s in S , phi_s $ is true
\
  - For each tree $T$ of the form  #tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$)), \ \
    if $phi_(t_1) " ," phi_(t_2) " ," phi_(t_3) " ," ... " ," phi_(t_(n-1)) "and" phi_(t_n)$ are all true, \
    
    then $phi_T$ is also true.
  
  // - $forall p in S ,\ 
  //   forall "trees" t_1, t_2, t_3, ..., t_(n-1),t_n "over" S ,\ 
  //   ( 
  //     phi_(t_1) "and" phi_(t_2) "and" phi_(t_3) "and" ... "and" phi_(t_(n-1)) "and" phi_(t_n) 
  //   ) 
  //   => phi_tau, \
  //   "where" tau #e #tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))
  //   $
  \
  then $phi_t$ is true for each tree $t$ over $S$.
]

//todo examples of trees over various sets

== Structural Recursion

We can also define functions on trees using a certain style of recursion.

From the definition of @definition_of_tree, we know that trees are 
- either of the form $s in S$
- or of the form #tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))

So, to define any function $( f : "Trees over" S -> X )$, we can divide taking the input into two cases, and define the outputs respectively.

Let's use this principle to define the function $ "size" : "Trees over" S -> NN $ which is meant to give the number of times the elements of $S$ appear in a tree over $S$.

$"size"(s) #d 1 \ "size"(#tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))) #d 1 + "size"(t_1) + "size"(t_2) + "size"(t_3) + ... + "size"(t_(n-1)) + "size"(t_n)$

== Termination

Using @definition_of_structural_induction_for_trees, let us prove that \
#proof(thm:[The definition of the function $"size"$ terminates on any finite tree.])[
  For each tree $t$, let $phi_t$ be the statement 
  #align(center)[" The definition of $"size"$ terminates on $t$"]
  To apply @definition_of_structural_induction_for_trees, we need only prove the 2 requirements and we'll be done. So let's do that -
  
  - *$angle.l angle.l" "forall s in S , phi_s "is true"" "angle.r angle.r $* \ 
      $"size"(s) #d 1$, so the definition terminates immediately.
  
  - *$angle.l angle.l " For each tree " $T$" of the form " . . . " then " phi_T" is also true" angle.r angle.r $* \ 
    Assume that each of $phi_(t_1)," "phi_(t_2)," "phi_(t_3), . . . ," "phi_(t_(n-1))," "phi_(t_n)$ is true. \
    That means that each of $"size"(t_1)," ""size"(t_2)," ""size"(t_3), . . . ," ""size"(t_(n-1))," ""size"(t_n)$ will terminate.

    Now, $"size"(T) #d 1 + "size"(t_1) + "size"(t_2) + "size"(t_3) + ... + "size"(t_(n-1)) + "size"(t_n)$
    
    Thus, we can see that each term in the right-hand side terminates.\ Therefore, the left-hand side "$"size"(tau)$",\ being defined as a well-defined combination of these terms,\ must also terminate.

    Hence $phi_(T)$ is proved!

  Hence the theorem is proved!!
]

= Why Trees?

But why care so much about trees anyway? Well, that is mainly due to the previously mentioned fact - "*In fact,any object in Haskell is internally modelled as a tree-like structure.*"

But why would Haskell choose to do that? There is a good reason, as we are going to see. //syntax trees

== The Problem

Suppose we are given that $x #e 5$ and then asked to find out the value of the expression $x^3 dot x^5 + x^2 + 1$.

How can we do this?

Well, since we know that $x^3 dot x^5 + x^2 + 1$ is the function $+$ applied to the inputs $x^3 dot x^5$ and $x^2 + 1$, \ we can first find out the values of these inputs and then apply $+$ on them!

Similarly, as long as we can put an expression in the form $f(x_1,x_2,x_3,...,x_(n-1),x_n)$, we can find out its value by finding out the values of its inputs and then applying $f$ on these values.

So, for dumb Haskell to do this (figure out the values of expressions, which is quite an important ability) , a vital requirement is to be able to easily put expressions in the form $f(x_1,x_2,x_3,...,x_(n-1),x_n)$.

But this can be quite difficult - In $x^3 dot x^5 + x^2 + 1$, it takes our human eyes and reasoning to figure it out fully, and for long, complicated expressions it will be even harder.

== The Solution

One way to make this easier to represent the expression in the form of a tree - 

For example, if we represent  $x^3 dot x^5 + x^2 + 1$ as $ #tree(spread:1.5,($+$, $x^3 dot x^5$, $x^2 + 1$)) $, it becomes obvious what the function is and what the inputs are to which it is applied.
\ \ \ 
In general, we can represent the expression $f(x_1,x_2,x_3,...,x_(n-1),x_n)$ as 
$
#tree(($f$,$x_1$,$x_2$,$x_3$,dots,$x_(n-1)$,$x_n$))                                    
$
\ \ \ 
But why stop there, we can represent the sub-expressions ( such as $x^3 dot x^5$ and $x^2 + 1$ ) as trees too -
$
  #tree(spread:1.5,($+$, ($dot$,$x^3$,$x^5$), ($+$,$x^2$,$1$)))
$
\ \
and their sub-expressions can be represented as trees as well -
$
  #tree(spread:1.5,pad:0.25,($+$, ($dot$,($(" ")^3$,$x$),($(" ")^5$,$x$)), ($+$,($(" ")^2$,$x$),$1$)))
$

This is known as the as an Abstract Syntax Tree, and this is (approximately) how Haskell stores expressions, i.e., how it stores everything.

#def(sub: "abstract syntax tree")[
  The *abstract syntax tree of a well-formed expression* is defined by applying the "function" _$"AST"$_ to the expression. \ \
  The "function" _$"AST"$_ is defined as follows - \ \
  $"AST" : "Expressions" -> "Trees over values and variables"$\ \
  $"AST"(v) #d v, "if "v" is a value or variable"$\  \
  $"AST"(f(x_1,x_2,x_3,...,x_(n-1),x_n)) #d\ "                         "#tree(spread:2, grow:1.5,pad:0.4,($f$,$"AST"(x_1)$,$"AST"(x_2)$,$"AST"(x_3)$,dots,$"AST"(x_(n-1))$,$"AST"(x_n)$))$
]