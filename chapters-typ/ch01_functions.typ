#import "../Modules/Definition.typ" : def
#import "../Modules/Proof.typ" : proof
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Tree.typ" : tree, dots
#let d = sym.colon.eq
#let e = $stretch(=, size: #120%)$
#let gray(x) = text(fill: luma(150), x) 
#let grad(content) = {
  set text(fill: gradient.linear(..color.map.flare))
  box(content)
}
#let love(content) = {
  set text(fill: gradient.linear(rgb(214,2,112), rgb(0, 56, 168), angle: -30deg))
  // Yes, these are bi-flag colors, tried a bunch, this looked the prettiest
  box(content)
}

= Precise Communication 
// (better name suggestion always welcome) 
// lmao better 'better name suggestions' is associative. 

Haskell (as well as a lot of other programming languages) and Mathematics, both involve communicating an idea in a language that is precise enough for them to be understood without ambiguity.

The main difference between mathematics and haskell is *who* reads what we write.

When writing any form of mathematical expression, it is the expectation that it is meant to be read by humans, and convince them of some mathematical proposition.\
On the other hand, haskell code is not _primarily_ meant to be read by humans, but rather by machines. The computer reads haskell code, and interprets it into steps for manipulating some expression, or doing some action.

When writing mathematics, we can choose to be a bit sloppy and hand-wavy with our words, as we can rely to some degree on the imagination and pattern-sensing abilities of the reader to fill in the gaps.

However, in the context of Haskell, computers, being machines, are extremely unimaginative, and do not possess any inherent pattern-sensing abilities. Unless we spell out the details for them in excruciating detail, they are not going to understand what we want them to do.

Since in this course we are going to be writing for computers, we need to ensure that our writing is very precise, correct and generally *idiot-proof*. (Because, in short, computers are idiots)

In order to practice this more formal style of writing required for *haskell code*, the first step we can take is to know how to *write our familiar mathematics more formally*.

= The Building Blocks

The language of writing mathematics is fundamentally based on two things -
- *Symbols:* such as $0,1,2,3,x,y,z,n,alpha,gamma,delta,NN,QQ,RR,in,<,>,f,g,h,=>,forall,exists$ etc. Along with;
- *Expressions:* which are sentences or phrases made by chaining together these symbols, such as 
  - $x^3 dot x^5 + x^2 + 1$
  - $f(g(x,y),f(a,h(v),c),h(h(h(n))))$
  - $forall alpha in RR " " exists L in RR " " forall epsilon > 0 " " exists delta > 0 " " | x - alpha | < delta => | f(x) - f (alpha) | < epsilon$
  #v(5pt,weak:true) etc.

= Values

#def(sub : "mathematical value")[
  A *mathematical value* is a single and specific well-defined mathematical object that is constant, i.e., does not change from scenario to scenario nor represents an arbitrary object.

  The following examples should clarify further.
]

Examples include - 
- The real number $pi$
// - The set of rational numbers $QQ$
- The order $<$ on $NN$
- The function of squaring a real number $: RR -> RR$
- The number $d$ , defined as the smallest number in the set \ ${n in NN | exists "infinitely many pairs "(p,q)" of prime numbers with "|p-q| <= n}$

Therefore we can see that relations and functions can also be *values*, as long as they are specific and not scenario-dependent. For example, the order $<$ on $NN$ does not have different meanings or interpretations in different scenarios, but rather has a fixed meaning which is independent of whatever the context is. 

In fact, as we see in the last example, we don't even currently know the exact value of $d$. \ The famous "Twin Primes Conjecture" is just about whether $d==2$ or not.

So, the moral of the story is that even if we don't know what the exact value is,\ we can still know that it is *some @definition_of_mathematical_value*,\ as it does not change from scenario to scenario and remains constant, even though it is an unknown constant.

= Variables

#def(sub : "mathematical variable")[
A *mathematical variable* is a symbol or chain of symbols\ #v(0.4em,weak:true) meant to represent an arbitrary element from a set of @definition_of_mathematical_value$"s"$, \ 
usually as a way to show that whatever process follows is general enough so that the process can be carried out with any arbitrary value from that set.

The following examples should clarify further.
]

For example, consider the following function definition - 
$
  f : RR &-> RR \
  f(x) &:= 3x + x^2
$
Here, $x$ is a @definition_of_mathematical_variable as it isn't any one specific @definition_of_mathematical_value, \ but rather *represents an arbitrary* element from the set of real numbers.

Consider the following theorem - \
#proof(thm:[Adding $1$ to a natural number makes it bigger.])[
  Take $n$ to be an arbitrary natural number.\
  We know that $1 > 0$.\
  Adding $n$ to both sides of the preceding inequality yields $ n + 1 > n $
  Hence Proved !!
]
Here, $n$ is a @definition_of_mathematical_variable as it isn't any one specific @definition_of_mathematical_value, \ but rather *represents an arbitrary* element from the set of natural numbers.

Here is another theorem -
#proof(thm:[For any $f : NN -> NN$ , if f is a strictly increasing function, then f(0) < f(1)])[
  Let $f : NN -> NN$ be a strictly increasing function.
  Thus $ forall n,m in NN, n < m => f(n) < f(m) $
  Take $n$ to be $0$ and $m$ to be $1$.
  Thus we get $ f(0) < f(1) $
  Hence Proved!
]
Here, $f$ is a @definition_of_mathematical_variable as it isn't any one specific @definition_of_mathematical_value, \ but rather *represents an arbitrary* element from the set of all $NN -> NN$ strictly increasing functions.

It has been used to show a certain fact that holds for *any* natural number.

= Well-Formed Expressions <math_expression>

Consider the expression - $ x y x<== forall => f (  <~> arrow(v)  $
It is an expression as it *is* a bunch of symbols arranged one after the other, but the expression is obviously meaningless.\

So what distinguishes a meaningless expression from a meaningful one? Wouldn't it be nice to have a systematic way to check  whether an expression is meaningful or not?\

Indeed, that is what the following definition tries to achieve - a systematic method to detect whether an expression is well-structured enough to possibly convey any meaning.

#def(sub:"checking whether mathematical expression is well-formed")[
  It is difficult to give a direct definition of a *well-formed expression*. 
  
  So before giving the direct definition,\  we define a _formal procedure_ to check whether an expression is a *well-formed expression* or not.

  The procedure is as follows - 

  Given an expression _$e$_, 
  
    - first check whether _$e$_ is 
      - a @definition_of_mathematical_value, or
      #v(0pt,weak:true)
      - a @definition_of_mathematical_variable
      #v(5pt,weak:true) in which cases _$e$_ passes the check and is a *well-formed expression*.
   
    Failing that,
 
    - check whether _$e$_ is of the form _$f(e_1,e_2,e_3,...,e_n)$_, where 
      - _$f$_ is a function
      - which takes _$n$_ inputs, and
      #v(5pt,weak:true)
      - _$e_1,e_2,e_3,...,e_n$_ are all _well-formed expressions_ which are _valid inputs_ to _$f$_.

    And only if _$e$_ passes this check will it be a *well-formed expression*.]

// #def(sub:"well-formed mathematical expression")[
//   *Well-formed expressions*, #love[like love], is one of those things which is easier to identify than to describe.

//   The following is a procedure to check if a given expression $e$ is *well-formed*: 
//     - first check whether _$e$_ is a: 
//       - @definition_of_mathematical_value, or;
//       - @definition_of_mathematical_variable
//       in which cases _$e$_ passes the check and is an expression, otherwise;
//     - check whether _$e$_ is of the form _$f(e_1,e_2,e_3,...,e_n)$_, where 
//       - _$f$_ is a function #text(size:0.8em)[(which can be either a @definition_of_mathematical_value or @definition_of_mathematical_variable)]
//       - which takes _$n$_ inputs, 
//       #v(5pt,weak:true) and #v(5pt,weak:true)
//       - _$e_1$,$e_2$,$e_3$...$e_n$_ are all _well-formed expressions_ which are _valid inputs_ to _$f$_.

// Now we can define a *Well-formed expressions* as any expression that satisfies our procedure.
// ] // I think this looks better? 

#def(sub:"well-formed mathematical expression")[
  A _mathematical expression_ is said to be a  *well-formed mathematical expression* if and only if it passes the formal checking procedure defined in @definition_of_checking_whether_mathematical_expression_is_well-formed.
]

// *Remark:* (the function $f$ can be a @definition_of_mathematical_value or @definition_of_mathematical_variable)

Let us use @definition_of_checking_whether_mathematical_expression_is_well-formed to check if $x^3 dot x^5 + x^2 + 1$ is a well-formed expression. \
( We will skip the check of whether something is a valid input or not, as that notion is still not very well-defined for us. )

$x^3 dot x^5 grad(+) x^2 + 1$ is $+$ applied to the inputs $x^3 dot x^5$ and $x^2 + 1$. \
Thus we need to check that $x^3 dot x^5$ and $x^2 + 1$ are well-formed expressions which are valid inputs to $+$.

$x^3 grad(dot) x^5$ is $dot$ applied to the inputs $x^3$ and $x^5$. \
Thus we need to check that $x^3$ and $x^5$ are well-formed expressions.

$x^grad(3)$ is $(" ")^3$ applied to the input $x$. \
Thus we need to check that $x$ is a well-formed expression.

$x$ is a well-formed expression, as it is a @definition_of_mathematical_variable.

$x^grad(5)$ is $(" ")^5$ applied to the input $x$. \
Thus we need to check that $x$ is a well-formed expression.

$x$ is a well-formed expression, as it is a @definition_of_mathematical_variable.

$x^2 grad(+) 1$ is $+$ applied to the inputs $x^2$ and $1$. \
Thus we need to check that $x^2$ and $1$ are well-formed expressions.

$x^grad(2)$ is $(" ")^2$ applied to the input $x$. \
Thus we need to check that $x$ is a well-formed expression.

$x$ is a well-formed expression, as it is a @definition_of_mathematical_variable.

$1$ is a well-formed expression, as it is a @definition_of_mathematical_value.

Done!

#exercise(sub:"checking whether expression is well-formed")[
  Suppose $a,b,v,f,g$ are @definition_of_mathematical_value#[s].\
  Suppose $x,y,n,h$ are @definition_of_mathematical_variable#[s].

  Check whether the expression $ f(g(x,y),f(a,h(v),c),h(h(h(n)))) $ is well-formed or not.
]

= Function Definitions

Functions are a very important tool in mathematics and they form the foundations of Haskell programming. \ 
Thus, it is very helpful to have a deeper understanding of *how function definitions in mathematics work*.

== Using Expressions

In its simplest form, a function definition is made up of a left-hand side, '$#d$' in the middle#footnote[
  In order to have a clear distinction between definition and equality,\
  we use $A #d B$ to mean "$A$ is defined to be $B$",\
  and we use $A #e B$ to mean "$A$ is equal to $B$".
], and a right-hand side.

A few examples -
- $f(x) #d x^3 dot x^5 + x^2 + 1$
- $"second"(a,b) #d b$
- $zeta(s) #d sum_(n=1)^oo 1/n^s$

On the left we write the name of the function followed by a number of variables which represent its inputs.

In the middle we write '$#d$', indicating that right-hand side is the definition of the left-hand side.

On the right, we write a @definition_of_well-formed_mathematical_expression using the variables of the left-hand side, describing to how to combine and manipulate the inputs to form the output of the function.

== Some Conveniences

Often in the complicated definitions of some functions, the right-hand side expression can get very convoluted, so there are some conveniences which we can use to reduce this mess.

=== Where, Let

Consider the definition of the famous $"sine"$ function - \ 
\
$ "sine" : RR -> RR $
Given an angle $theta$,\
Let $T$ be a right-angled triangle, one of whose angles is $theta$.\
Let $p$ be the length of the perpendicular of $T$.\
Let $h$ be the length of the hypotenuse of $T$.\
Then $ "sine"(theta) #d p/h $

Here we use the variables $p$ and $h$ in the right-hand side of the definition, but to get their meanings one will have to look at how they are defined beforehand in the lines beginning with "let".

We can also do the exact same thing using "where" instead of "let".

$ 
  "sine" : RR &->  RR \
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

for convenience, can be rewritten as - 

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
  "signum" : RR &-> RR\
  "signum"(x) &#d cases( 
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
Given any input, we start reading from the topmost line of the function definition to the bottom-most, and we apply the first applicable definition.

So here, the first line #text(fill:blue)[($"signum"(0) &#d 0$)] will be used as the definition when the input is $0$.

== Recursion <math-recursion>

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

=== Induction <induction>

#def(sub:"principle of mathematical induction")[
  Suppose we have an infinite sequence of statements $phi_0,phi_1, phi_2, phi_3, . . . $\
  and we can prove the following 2 statements -
  - $phi_0$ is true
  - $"For each "n" > 0, if "phi_(n-1)" is true, then "phi_(n)" is also true."$
  then all the statements $phi_0,phi_1, phi_2, phi_3, . . . $ in the sequence are true.
]

The above definition should be read as follows, given a sequence of formulas:
- The first one is true.
- Any formula being true, implies that the next one in the sequence is true.
Then all of the formulas in the sequence are true. Something like a chain of dominoes falling.

#exercise[Show that $n^2$ is the same as the sum of first $n$ odd numbers using induction.]

#exercise(sub : "The scenic way")[
   (a) Prove the following theorem of Nicomachus by induction:
$
1^3 = 1\
2^3 = 3 + 5\
3^3 = 7 + 9 + 11\
4^3 = 13 + 15 + 17 + 19 \
.\
.\
.\
$ 
(b) Use this result to prove the remarkable formula
$
  1^3 + 2^3 + dots + n^3 = (1+2+dots+n)^2
$ 
]

#exercise(sub : "There is enough information!")[
  Given $a_0 = 100$ and $a_n = - a_(n-1) - a_(n-2)$, what is $a_2025$?
]

#exercise(sub : "2-3 Color Theorem")[
  A k-coloring is said to exist if the regions the plane is divided off in can be colored with three colors in such a way that no two regions sharing some length of border are the same color.
  
  (a) A finite number of circles (possibly intersecting and touching) are drawn on a paper. Prove that a valid 2-coloring of the regions divided off by the circles exists.

  (b) A circle and a chord of that circle are drawn in a plane. Then a second circle
and chord of that circle are added. Repeating this process, until there are n
circles with chords drawn, prove that a valid 3-coloring of the regions in the plane divided off by
the circles and chords exists.
]

#exercise(sub:"Square-full")[
  Call an integer square-full if each of its prime factors occurs to a second power (at least). Prove that there are infinitely many pairs of consecutive square-fulls.

  Hint: We recommend using induction. Given $(a,a+1)$ are square-full, can we generate another?
]

#exercise(sub : "Same Height?")[
Here is a proof by induction that all people have the same height. We prove that for any positive integer $n$, any group of $n$ people all have the same height. This is clearly true for $n = 1$. Now assume it for $n$, and suppose we have a group of $n + 1$ persons, say $P_1, P_2,dots, P_(n+1)$. By the induction hypothesis, the $n$ people $P_1, P_2, dots, P_n$ all have the same height. Similarly the $n$ people $P_2, P_3, dots, P_(n+1)$ all have the same height. Both groups of people contain $P_2, P_3, dots, P_n$, so $P_1$ and $P_(n+1)$ have the same height as $P_2, P_3, dots, P_n$. Thus all of $P_1, P_2,dots, P_(n+1)$ have the same height. Hence by induction, for any $n$ any group of $n$ people have the same height. Letting $n$ be the total number of people in the world, we conclude that all people have the same height. Is there a flaw in this argument?
]

#exercise(sub:"proving the principle of induction")[
  Prove that the following statements are equivalent - \
  - every nonempty subset of $NN$ has a smallest element
  - the @definition_of_principle_of_mathematical_induction
  You can assume that $<$ is a linear order on $NN$\ such that there are no elements strictly between $n$ and $n+1$.
]

=== Proving Termination using Induction

So let's see the @definition_of_principle_of_mathematical_induction in action, and use it to prove that 
#proof(thm:[The definition of the fibonacci function $F$ terminates for any natural number $n$.])[
  For each natural number $n$, let $phi_n$ be the statement 
  #align(center)[" The definition of $F$ terminates for every natural number which is $<= n" "$"]
  To apply the @definition_of_principle_of_mathematical_induction, we need only prove the 2 requirements and we'll be done. So let's do that -
  
  - *$angle.l angle.l" "phi_0" is true "angle.r angle.r $* \ 
    The only natural number which is $<=0$ is $0$, and $F(0) #d 1$, so the definition terminates immediately.
  
  - *$angle.l angle.l" For each "n" > 0, if "phi_(n-1)" is true, then "phi_(n)" is also true. "angle.r angle.r $* \ 
    Assume that $phi_(n-1)$ is true. \
    Let $m$ be an arbitrary natural number which is $<= n.$ \ 

    - $angle.l angle.l" "$ Case 1 $" "(m<=1)$ $" "angle.r angle.r$ \
      $F(m) #d 1$, so the definition terminates immediately.

    - $angle.l angle.l" "$ Case 2 $" "(m>1)$ $" "angle.r angle.r$ \
      $F(m) #d F(m-1) + F(m-2)$, \
      and since $m-1$ and $m-2$ are both $<=n-1$, \
      $phi_(n-1)$ tells us that both $F(m-1)$ and $F(m-2)$ must terminate. \
      Thus $F(m) := F(m-1) + F(m-2)$ must also terminate. 
    
    Hence $phi_(n)$ is proved!
  Hence the theorem is proved!!
]

= Infix Binary Operators

Usually, the name of the function is written before the inputs given to it. For example, we can see that in the expression $f(x,y,z)$, the symbol $f$ is written to the left of / before any of the inputs $x,y$ or $z$.

However, it's not always like that. For example, take the expression $ x+y $ Here, the function name is $+$ , and the inputs are $x$ and $y$. 

But $+$ has been written in-between $x$ and $y$, not before!

Such a function is called an infix binary operator#footnote[\ infix - because the function name is *in-between* the inputs \ binary - because exactly *$2$* inputs, and binary refers to *$2$* \ operator - another way of saying *function*]

#def(sub:"infix binary operator")[ An *infix binary operator* is a _function_ which takes exactly $2$ inputs and whose function name is written between the $2$ inputs rather than before them. ]

Examples include - \
- $+$ (addition)
- $-$ (subtraction)
- $times$ or $*$ (multiplication)
- $\/$ (division)

= Trees

Trees are a way to structure a collection of objects. 

Trees are a fundamental way to understand expressions and how haskell deals with them.

*In fact, any object in Haskell is internally modelled as a tree-like structure.*

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
\ \ \ \
*In general*, if we have an object $ p $ and a bunch of trees $ t_1,t_2,t_3,...,t_(n-1),t_n $, we can put them together in a larger tree, by connecting them with $n$ line segments, like so - 
$
  #tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))
$

We would like to define trees so that only those which are made in the above manner qualify as trees.

== Formal Definition of Trees

A *tree over a set $S$* defines a meaningful structure on a collection of elements from $S$. \ The examples we've seen include trees over the set $NN$, as well as a tree over the set of English words.

We will adopt a similar approach to defining trees as we did with expressions, i.e., we will provide a formal procedure to check whether a mathematical object is a tree, rather than directly defining what a tree is.

#def(sub:"checking whether object is tree")[

  The formal procedure to determine whether an object is a *tree over a set $S$* is as follows -

  Given a mathematical object _$t$_,

  - first check whether _$t in S$_, in which case _$t$_ passes the check, and is a *tree over $S$*

  Failing that,

  - check whether _$t$_ is of the form _#tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))_, where

    - _$p in S$_
    - and each of _$t_1, t_2, t_3, ..., t_(n-1),"and" t_n$_ is a *tree over $S$*.

]

#def(sub:"tree")[
  Given a set _$S$_, a _mathematical object_ is said to be a *tree over $S$* if and only if it passes the formal checking procedure defined in @definition_of_checking_whether_object_is_tree.
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
  Suppose for each tree $t$ over a set $S$, we have a statement $phi_t$ .\
 
  If we can prove the following two statements  -

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
  then $phi_t$ is true for all trees $t$ over $S$.
]

//todo examples of trees over various sets

== Structural Recursion

We can also define functions on trees using a certain style of recursion.

From the definition of @definition_of_tree, we know that trees are 
- either of the form $s in S$
- or of the form #tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))

So, to define any function $( f : "Trees over" S -> X )$, we can divide taking the input into two cases, and define the outputs respectively.

#def(sub:"tree size")[
  Let's use this principle to define the function $ "size" : "Trees over" S -> NN $ which is meant to give the number of times the elements of $S$ appear in a tree over $S$. \ \

  #set text(size :0.9em)

  $"size"(s) #d 1\ 
  "size"(#tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))) #d 1 + "size"(t_1) + "size"(t_2) + "size"(t_3) + ... + "size"(t_(n-1)) + "size"(t_n)$\
]

== Termination

Using @definition_of_structural_induction_for_trees, let us prove that \
#proof(thm:[The definition of the function "$"size"$" terminates on any tree.])[
  For each tree $t$, let $phi_t$ be the statement 
  #align(center)[" The definition of $"size"(t)$ terminates "]
  To apply @definition_of_structural_induction_for_trees, we need only prove the 2 requirements and we'll be done. So let's do that -
  
  - *$angle.l angle.l" "forall s in S , phi_s "is true"" "angle.r angle.r $* \ 
      $"size"(s) #d 1$, so the definition terminates immediately.
  
  - *$angle.l angle.l " For each tree " $T$" of the form " . . . " then " phi_T" is also true" angle.r angle.r $* \ 
    Assume that each of $phi_(t_1)," "phi_(t_2)," "phi_(t_3), . . . ," "phi_(t_(n-1))," "phi_(t_n)$ is true. \
    That means that each of $"size"(t_1)," ""size"(t_2)," ""size"(t_3), . . . ," ""size"(t_(n-1))," ""size"(t_n)$ will terminate.

    Now, $"size"(T) #d 1 + "size"(t_1) + "size"(t_2) + "size"(t_3) + ... + "size"(t_(n-1)) + "size"(t_n)$
    
    Thus, we can see that each term in the right-hand side terminates.\ Therefore, the left-hand side "$"size"(T)$",\ being defined as an addition of these terms,\ must also terminate. \  (since addition of finitely many terms always terminates)

    Hence $phi_(T)$ is proved!

  Hence the theorem is proved!!
]

#exercise(sub:"tree depth")[
  Fix a set $S$.
  #def(sub:"tree depth")[
    $"depth" : "Trees over" S -> NN \
    "depth"(s) #d 1\ 
    "depth"(#tree(($p$,$t_1$,$t_2$,$t_3$,dots,$t_(n-1)$,$t_n$))) #d 1 + limits("max")_(1<=i<=n){"depth"(t_i)}$\
  ]
  + Prove that the definition of the function "$"depth"$" terminates on any tree over $S$.

  + Prove that for any tree $t$ over the set $S$, $ "depth"(t) <= "size"(t) $

  + When is $"depth"(t) == "size"(t)$ ?
]

#exercise()[
  This exercise is optional as it can be difficult, but it can be quite illuminating to understand the solution. So even if you don't solve it, you should ask for a solution from someone.

  Using the @definition_of_principle_of_mathematical_induction, \ prove @definition_of_structural_induction_for_trees.
]

= Why Trees? <why>

But why care so much about trees anyway? Well, that is mainly due to the previously mentioned fact - "*In fact, any object in Haskell is internally modelled as a tree-like structure.*"

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

== Exercises

All the following exercises are optional, as they are not the most relevant for concept-building. They are just a collection of problems we found interesting and arguably solvable with the theory of this chapter. Have fun!

#exercise(sub :"Turbo The Snail(IMO 2024, P5)")[
Turbo the snail is in the top row of a grid with $s >= 4$ rows and $s-1$ columns and
wants to get to the bottom row. However, there are $s-2$ hidden monsters, one in
every row except the first and last, with no two monsters in the same column.
Turbo makes a series of attempts to go from the first row to the last row. On
each attempt, he chooses to start on any cell in the first row, then repeatedly moves
to an orthogonal neighbor. (He is allowed to return to a previously visited cell.) If
Turbo reaches a cell with a monster, his attempt ends and he is transported back to
the first row to start a new attempt. The monsters do not move between attempts,
and Turbo remembers whether or not each cell he has visited contains a monster. If
he reaches any cell in the last row, his attempt ends and Turbo wins.

Find the smallest integer $n$ such that Turbo has a strategy which guarantees being
able to reach the bottom row in at most $n$ attempts, regardless of how the monsters
are placed.
]

#exercise(sub:"Points in Triangle")[
  Inside a right triangle a finite set of points is given. Prove that these points can be connected
by a broken line such that the sum of the squares of the lengths in the broken line is less than
or equal to the square of the length of the hypotenuse of the given triangle.
]

#exercise(sub:"Joining Points(IOI 2006, 6)")[
  A number of red points and blue points are drawn in a unit square with the following
properties:
- The top-left and top-right corners are red points.
- The bottom-left and bottom-right corners are blue points.
- No three points are collinear.
Prove it is possible to draw red segments between red points and blue segments between blue points
in such a way that: all the red points are connected to each other, all the blue points are connected
to each other, and no two segments cross.

As a bonus, try to think of a recipe or a set of instructions one could follow to do so.

Hint: Try using the 'trick' you discovered in @exercise_of_Points_in_Triangle.
]

#exercise(sub:"Usmions(USA TST 2015, simplified)")[
  A physicist encounters $2015$ atoms called usamons. Each usamon either has one
electron or zero electrons, and the physicist can’t tell the difference. The physicist’s
only tool is a diode. The physicist may connect the diode from any usamon A to
any other usamon B. (This connection is directed.) When she does so, if usamon A
has an electron and usamon B does not, then the electron jumps from A to B. In
any other case, nothing happens. In addition, the physicist cannot tell whether an
electron jumps during any given step. The physicist's goal is to arrange the usamons in a line such that all the
charged usamons are to the left of the un-charged usamons, regardless of the number
of charged usamons. Is there any series of diode
usage that makes this possible?
]

#exercise(sub:"Battery")[
(a) There are $2n + 1 (n > 2)$ batteries. We don’t know which batteries are good
and which are bad but we know that the number of good batteries is greater
by 1 than the number of bad batteries. A lamp uses two batteries, and it
works only if both of them are good. What is the least number of attempts
sufficient to make the lamp work?

(b) The same problem but the total number of batteries is $2n (n > 2)$ and the
numbers of good and bad batteries are equal.]

#exercise(sub:"Seven Tries (Russia 2000)")[
  Tanya chose a natural number $X <= 100$, and Sasha is trying to guess this number. He can select two natural numbers $M$ and $N$ less than $100$ and ask about $gcd(X+M,N)$. Show that Sasha can determine Tanya's number with at most seven questions.

  Note: We know of atleast 5 ways to solve this. Some can be genralized to any number $k$ other than $100$, with $ceil(log_2(k))$ many tries, other are a bit less general. We hope you can find atleast $2$.
]

#exercise(sub:"The best (trollest) codeforces question ever!")[
Let $s(k)$ be sum of digits in decimal representation of positive integer $k$. Given two integers $1 <= m, n <= 1129$ and $n$, find two integers $1 <= a, b <=10^2230$ such that
- $s(a) >= n$
- $s(b) >= n$
- $s(a+b) <= m$

For Example

*Input1* : 6 5

*Output1* : 6 7

*Input2* : 8 16

*Output2* : 35 53
]

#exercise(sub:"Rope")[
  Given a $r times c$ grid with $0 <= n <= r * c$ painted cells, we have to arrange ropes to cover the grid. Here are the rules through example:
  #image("../images/rope.png")

  Figure out an algorithm/recipie to covering the grid using $n+1$ ropes leagally.

  Hint: Try to first do the $n=0$ case. Then $r = 1$ case, with arbitrary $n$. Does this help?
]

#exercise(sub: "n composite")[Given $N$, find $N$ consecutive integers that are all composite numbers.]
#exercise(sub : "Divided by 5^n")[Prove that for every positive integer $n$, there exists an $n$-digit number divisible by $5^n$, all of whose
digits are odd.]

#exercise(sub:"This was rated 2100? (Timofey's Colourbook Problem, Codeforces)")[

One of Timofey's birthday presents is a colourbook in the shape of an infinite plane. On the plane, there are $n$ rectangles with sides parallel to the coordinate axes. All sides of the rectangles have odd lengths. The rectangles do not intersect, but they can touch each other.

Your task is, given the coordinates of the rectangles, to help Timofey color the rectangles using four different colors such that any two rectangles that *touch each other by a side* have *different colors*, or determine that it is impossible.

For example,

#figure(
image("../images/Timofey's Rectangle.png", width: 75%, )
)
is a valid filling. Make an algorithm/recipe to fulfill this task.

PS: You will feel a little dumb once you solve it. 
]

#exercise(sub: "Seating")[
  Wupendra Wulkarni storms into the exam room. He glares at the students.
  
  "Of course you all sat like this on purpose. Don’t act innocent. I know you planned to copy off each other. Do you all think I'm stupid? Hah! I've seen smarter chairs.
  
  Well, guess what, darlings? I’m not letting that happen. Not on my watch. 
  
  Here’s your punishment - uh, I mean, assignment:
  
  You’re all sitting in a nice little grid, let’s say $n$ rows and $m$ columns. I’ll number you from 1 to $n dot m$, row by row. That means the poor soul in row $i$, 
  column $j$ is student number $(i  - 1) dot m + j$. Got it? 
  
  Now, you better rearrange yourselves so that none of you little cheaters ends up next to the same neighbor again. Side-by-side, up-down—any adjacent loser you were plotting with in the original grid? Yeah, stay away from them."
  
  Your task is this:
  Find a new seating chart (in general an algorithm/recipie), using n rows and m columns, using every number from 1 to $n dot m$ such that no two students who were neighbors in the original grid 
  are neighbors again. 
  
  And if you think it’s impossible, then prove it as Wupendra won't satisfy for anything less.
]

#exercise(sub : "Yet some more Fibonnaci Identity")[
  Fibonnaci sequence is defined as $F_0 = 0, F_1 = 1$ and $F_n = F_(n-1) + F_(n-2)$. 

  (i) Prove that
  $
    sum_(n=2)^oo arctan((-1)^n/(F_2n)) = 1/2 arctan(1/2)
  $
  
  Hint : What is this problem doing on this list of problems?

  (ii) Every natural number can be expressed uniquely as a sum of Fibonacci numbers where the Fibonacci numbers used in the sum are all distinct, and no two consecutive Fibonacci numbers appear.

  (iii) Evaluate
  $
    sum_(i=2)^oo 1/(F_(i-1)F_(i+1))
  $
]

#exercise(sub : "Round Robin")[
   A group of $n$ people play a round-robin chess tournament. Each match ends in either a win or a lost. 
   Show that it is possible to label the players $P_1, P_2, P_3, dots , P_n$ in such a way that $P_1$ defeated $P_2$, 
   $P_2$ defeated $P_3$, . . . , $P_(n−1)$ defeated $P_n$.
]

#exercise(sub : "Stamps")[
(i) The country of Philatelia is founded for the pure benefit of stamp-lovers. Each
year the country introduces a new stamp, for a denomination (in cents) that cannot
be achieved by any combination of older stamps. Show that at some point the
country will be forced to introduce a 1-cent stamp, and the fun will have to end.

(ii) Two officers in Philatelia decide to play a game. They alternate in issuing stamps. The first officer to name 1 or a sum of some previous numbers (possibly with repetition) loses. Determine which player has the winning strategy.
]

#exercise(sub : "Seven Dwarfs")[
The Seven Dwarfs are sitting around the breakfast table;
Snow White has just poured them some milk. Before they drink, they perform a little
ritual. First, Dwarf 1 distributes all the milk in his mug equally among his brothers’
mugs (leaving none for himself). Then Dwarf 2 does the same, then Dwarf 3, 4, etc.,
finishing with Dwarf 7. At the end of the process, the amount of milk in each dwarf’s
mug is the same as at the beginning! What was the ratio of milt they started with?
]

#exercise(sub : "Coin Flip Scores")[
  A gambling graduate student tosses a fair coin and scores one point for each head that
turns up and two points for each tail. Prove that the probability of the student scoring
exactly n points at some time in a sequence of n tosses is $(2 + (−1/2)^n)/3$
]

#exercise(sub : "Coins (IMO 2010 P5)")[
  Each of the six boxes $B_1$, $B_2$, $B_3$, $B_4$, $B_5$, $B_6$ initially contains one coin. The following operations are allowed

(1) Choose a non-empty box $B_j$, $1 <= j <= 5$, remove one coin from $B_j$ and add two coins to $B_(j+1)$;

(2) Choose a non-empty box $B_k$, $1 <= k <= 4$, remove one coin from $B_k$ and swap the contents (maybe empty) of the boxes $B_(k+1)$ and $B_(k+2)$.

Determine if there exists a finite sequence of operations of the allowed types, such that the five boxes $B_1$, $B_2$, $B_3$, $B_4$, $B_5$ become empty, while box $B_6$ contains exactly $2010^(2010^2010)$ coins.
]

#exercise(sub : "Caves (IOI 2013, P4)")[
While lost on the long walk from the college to the UQ Centre, you have stumbled across the entrance to a secret cave system running deep under the university. The entrance is blocked by a security system consisting of $N$ consecutive doors, each door behind the previous; and $N$ switches, with each switch connected to a different door.

The doors are numbered $0, 1, dots, 4999$ in order, with door $0$ being closest to you. The switches are also numbered $0, 1, dots, 4999$, though you do not know which switch is connected to which door.

The switches are all located at the entrance to the cave. Each switch can either be in an up or down position. Only one of these positions is correct for each switch. If a switch is in the correct position then the door it is connected to will be open, and if the switch is in the incorrect position then the door it is connected to will be closed. The correct position may be different for different switches, and you do not know which positions are the correct ones.

You would like to understand this security system. To do this, you can set the switches to any combination, and then walk into the cave to see which is the first closed door. Doors are not transparent: once you encounter the first closed door, you cannot see any of the doors behind it. You have time to try $70,000$ combinations of switches, but no more. Your task is to determine the correct position for each switch, and also which door each switch is connected to.
]

#exercise(sub : "Carnivel (CEIO 2014)")[
  Each of Peter’s $N$ friends (numbered from $1$ to $N$) bought exactly one carnival costume in order to wear it at this year’s carnival parties. There are $C$ different kinds of costumes,numbered from $1$ to $C$. Some of Peter’s friends, however, might have bought the same kind of costume. Peter would like to know which of his friends bought the same costume. For this purpose, he organizes some parties, to each of which he invites some of his friends. 
  
  Peter knows that on the morning after each party he will not be able to recall which costumes he will have seen the night before, but only how many different kinds of costumes he will have seen at the party. Peter wonders if he can nevertheless choose the guests of each party such that he will know in the end, which of his friends had the same kind of costume. Help Peter!

  Peter has $N <= 60$ friends and we can not have more than $365$ parties(as we want to know the costumes by the end of the year).
]


// cite
// @misc{noauthor_travelproblemset2015pdf_nodate,
// 	title = {{TravelProblemSet2015}.pdf},
// 	url = {https://drive.google.com/file/d/1r5qkXbhLdZWRlMdO9ogVwBe6a8742RAB/view?usp=sharing&usp=embed_facebook},
// 	urldate = {2025-06-28},
// 	journal = {Google Docs},
// 	file = {Snapshot:/Users/deepthought/Zotero/storage/Y3B7ICFE/view.html:text/html},
// }

// @book{knuth_art_1997,
// 	address = {Reading, Mass},
// 	edition = {3rd ed},
// 	title = {The art of computer programming},
// 	isbn = {978-0-201-89683-1 978-0-201-89684-8 978-0-201-89685-5},
// 	publisher = {Addison-Wesley},
// 	author = {Knuth, Donald Ervin},
// 	year = {1997},
// 	keywords = {Computer algorithms, Computer programming},
// 	annote = {Includes indexes},
// 	annote = {v. 1. Fundamental algorithms -- v. 2. Seminumerical algorithms -- v. 3. Sorting and searching},
// }

// @misc{noauthor_algorithmspdf_nodate,
// 	title = {Algorithms.pdf},
// 	url = {https://drive.google.com/file/d/19xueTWqPby2GJfTJrRomSg8Vh59pcWS4/view?usp=sharing&usp=embed_facebook},
// 	urldate = {2025-06-28},
// 	journal = {Google Docs},
// 	file = {Snapshot:/Users/deepthought/Zotero/storage/6UG8F599/view.html:text/html},
// }

// @misc{noauthor_2019-inductionpdf_nodate,
// 	title = {2019-induction.pdf},
// 	url = {https://drive.google.com/file/d/1_bxkIbd9tQ1w9qqFShClR9jKEscvoqFw/view?usp=embed_facebook},
// 	urldate = {2025-06-28},
// 	journal = {Google Docs},
// 	file = {Snapshot:/Users/deepthought/Zotero/storage/TK6AIQIL/view.html:text/html},
// }

// @book{larson_problem-solving_1990,
// 	address = {New York Berlin Paris [etc.]},
// 	series = {Problem books in mathematics},
// 	title = {Problem-solving through problems},
// 	isbn = {978-3-540-96171-0 978-0-387-96171-2},
// 	language = {eng},
// 	publisher = {Springer},
// 	author = {Larson, Loren C.},
// 	year = {1990},
// }

// @misc{noauthor_polya_2015,
// 	title = {The {Pólya} {Seminar}},
// 	url = {https://polyaseminar.wordpress.com/},
// 	abstract = {You think you have problems...},
// 	language = {en},
// 	urldate = {2025-07-05},
// 	journal = {The Pólya Seminar},
// 	month = oct,
// 	year = {2015},
// }

// @misc{noauthor_william_nodate,
// 	title = {The {William} {Lowell} {Putnam} competition and the {Polya} problem solving seminar: {Polya} {Problem} {Solving} {Seminar}},
// 	url = {https://canvas.stanford.edu/courses/46752/wiki},
// 	urldate = {2025-07-05},
// }

// @misc{noauthor_21-295_nodate,
// 	title = {21-295: {Putnam} {Seminar}},
// 	url = {https://www.math.cmu.edu/~ploh/2015-295.shtml},
// 	urldate = {2025-07-05},
// 	file = {21-295\: Putnam Seminar:/Users/deepthought/Zotero/storage/B5CX5UCT/2015-295.html:text/html},
// }

// @misc{noauthor_william_nodate-1,
// 	title = {The {William} {Lowell} {Putnam} {Mathematical} {Competition} and the {Polya} {Problem}-{Solving} {Seminars} 2007},
// 	url = {https://math.stanford.edu/~vakil/putnam07/},
// 	urldate = {2025-07-05},
// }

// @misc{noauthor_putnam_nodate,
// 	title = {Putnam seminar {\textbar} {Pat} {Devlin}},
// 	url = {https://campuspress.yale.edu/devlin/putnam-seminar/},
// 	urldate = {2025-07-05},
// }
