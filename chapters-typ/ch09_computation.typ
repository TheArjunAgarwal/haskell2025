#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate

#let definition = def
#let example = it => [For example - \ #it]

= Introduction
< to do >

= Asymptotics
== Big El
#definition(sub : "Big Ell notation")[
  The symbol $cal(L)$ means absolutly atmost. For example, $cal(L)(4)$ refers to a value whoose absolute value is less than or equal to $4$. For example, $pi = cal(L)(4)$. 
]

Note, the $=$ sign is not trasitive in this regard. The reason for this is because the notation came from math and unlike Computer scientists, mathematicains have a small vocabulary and couldn't think of another symbol. As a Mathematician once put in words(quite surprising),

#quote(sub : "De Bruijn")[Mathematicians customarily use the $=$ sign as they use the word “is” in English: Aristotle is a man, but a man isn’t necessarily Aristotle.]

You can also see this by the story, let's say a textbook author's typewriter doesn't have $>$ sign. So they start using $ 5 = cal(L)(3)$ to reprasent that $3$ is lesser than $5$. They can also write $cal(L)(3) = cal(L)(5)$ but not $cal(L)(5) = cal(L)(3)$

Comming back to mathematics, Big-A notation is very compatible with arithmatic.
#proof(thm : [
  + $pi = 3.14 + cal(L)(0.005)$
  + $10^(cal(L)(2)) = cal(L)(100)$
])[
  We will just use the defination of $-k <= A(k) <= k$.

  Thus, $pi = 3.14 + cal(L)(0.005) <==> pi in [3.14 - 0.05, 3.14 + 0.05]$ which is true as $3.140 < pi = 3.1415926 dots < 3.142$.

  Similerly, $10^(cal(L)(2)) = cal(L)(100) <==> [10^(-2), 10^(2)] subset.eq [-100, 100]$ which is true.
]

#proof(thm: [$cal(L)(x) dot cal(L)(y) = cal(L)(x y)$])[
  We can eliminate the $cal(L)$ to get that the above statement is equivalent to, $[-x y, x y] subset.eq [-x y, x y]$ which is trivially true.
]

We can use the notation for variables as well,
#proof(thm:[
+ $sin x=cal(L) (1);$
+ $cal(L)(x) =x dot cal(L)(1)$;
+ $cal(L)(x)+cal(L)(y) =cal(L)(x+y) $, where$ x, y >= 0$;
+ $(1+cal(L)(t))^2 =1+3cal(L)(t)$, where $t=A(1)$.
])[
Left as an excercise to reader.
]

#exercise(sub : "Big Ell Analysis")[
  + Simplify: $(5 + cal(L)(0.2)) + (3 + cal(L)(0.1))$
  + Simplify: $(5 + cal(L)(0.2)) * (3 + cal(L)(0.1))$ 
  + Express $e^x$ for $|x| <= 1$ in the form $1 + cal(L)(u)$ for some $u = k x$. 
    *Hint*: Use the fact that $e^x = 1 + x + x^2/2! + dots$.
  + Define a sequence $x_0 = $, $x_n = 2 x_(n-1) + A(epsilon_n)$ where $epsilon_n > -$. Then solve for $x_n$ when
    + $epsilon_n = 1/(3^n)$
    + $epsilon = 1/n^2$
]

== The Hierarchy of Functions
#definition(sub : "Asymptotic Dominance")[
 Asymptotic Dominance is a relation over functions, where $f(n) prec g(n)$ when $g(n)$ approaches infinity faster than $f(n)$. We can formalize this by saying:
$
f(n) prec g(n) <==> limsup_(n -> oo) (|f(n)|)/(|g(n)|) = 0
$
Furthermore, $f(n) asymp g(n) <==> limsup_(n -> oo) (|f(n)|)/(|g(n)|) = k$ where $0 < k < oo$ is constent.
]
#proof(thm : [Asymptotic dominence is transistive.])[
  Let $f(n) prec g(n)$ and $g(n) prec h(n)$. We wish to show $f(n) prec h(n)$.
  $
  limsup_(n -> oo) (|f(n)|)/(|h(n)|) = limsup_(n -> oo) (|f(n)|)/(|g(n)|) dot (|g(n)|)/(|h(n)|) = 0 dot 0 = 0
  $
]

#proof(thm : [If $alpha < beta$ then $x^(alpha) prec x^(beta)$])[
Left as an excercise to reader.
]
#proof(thm: [$f(n) prec g(n) <==> 1/(f(n)) succ 1/(g(n))$])[
  Left as an excercise to reader.
]
#proof(thm: [$e^(f(n)) prec e^(g(n)) <==> limsup_(n -> oo) (f(n) - g(n)) = - oo $])[
  Using the definition, we will get:
  $
  &e^(f(n)) prec e^(g(n))\
  <==>& limsup_(n -> oo) (e^(f(n)))/(e^(g(n))) = 0\
  <==>&  limsup_(n -> oo) e^(ln((e^(f(n)))/(e^(g(n))))) = 0\
  <==>& limsup_(n -> oo) e^(f(n) - g(n)) = 0\
  <==>& limsup_(n -> oo) f(n) - g(n) = - oo
  $
]
#proof(thm:[Let $f(x) = k * g(x)$ for some real $k != 0$, then $f(x) asymp g(x)$])[
Left as an excercise to reader.
]
#proof(thm:[Let $f(x) succ g(x)$ and $f(x) asymp h(x)$ thebm $f(x) asymp h(x) + g(x)$])[
  Left as an excercise to reader.
]

This previous last two theorems tells us that the equivalence induced by @definition_of_Asymptotic_Dominance is invarient under scaling and translation by dominated functions. That is, in a sum of functions, we only need to consider the most dominent function when comparing asymptotic dominence. This allows us to talk about a group of functions using a single 'reprasentative' function.

#definition(sub: "Reprasentive Function wrt asymptotic dominance")[
The representative function of a set of asymptotically equivalent functions is the simplest form of the equivalence class, defined as:

- A function with a leading coefficient of $1$,
- No additive subdominant terms (i.e., no terms in addition that are asymptotically dominated by others in the class).
]
For example $x^2$ is the reprasentative function of the equivalence class of quadratic functions, $a x^2 + b x + c$.

#proof(thm: [If $f(x),g(x)$ are the reprasentative functions of the equivalence class $F,G$; then show that $f(x) succ g(x) <==> hat(f)(x) succ hat(g)(x)$ for all $hat(f) in F$ and $hat(g) in G$])[
  ($==>$) Using the fact $f, hat(f) in F$, we get $f(x) asymp hat(f)(x) <==> lim_(x -> oo) (f(x))/(hat(f)(x)) = c_1$ where $c_1 != 0$ is a constent.

  Similerly, $g(x) asymp hat(g)(x) <==> lim_(x -> oo) (g(x))/(hat(g)(x)) = c_2$ where $c_2 != 0$ is a constent.

  Using $f(x) succ g(x) <==> lim_(x -> oo) (g(x))/(f(x)) = 0$

  Thus, 
  $
  limsup_(x -> oo) (|hat(g)(x)|)/(|hat(f)(x)|)\
  = limsup_(x -> oo) (|hat(g)(x)|)/(|g(x)|) dot (|g(x)|)/(|f(x)|) dot (|f(x)|)/(|hat(f)(x)|) \
  = 1/(c_2) dot 0 dot c_1 \
  = 0\
  => hat(f)(x) succ hat(g)(x)
  $

  ($<==$) $f in F$ and $g in g$ implies $f(x) succ g(x)$ as this is true for all functions in the equivalence clases.#footnote[We could just say this part was trivial.]
]

#exercise(sub : "An Hieraarchy of Common Functions")[
  Prove that:
  $
  1 prec log(log(n)) prec log(n) prec n^(epsilon) prec n^c prec n^(log(n)) prec c^n prec n! prec n^n prec c^(c^n)
  $
]

== Big Oh notation
These topics seem disconected, right? Let's fix that.
#definition(sub : "Big Oh (from Big Ell)")[
  $f(x) = cal(O)(g(x)) (n -> oo) <==> exists k, n_0 op("s.t.") forall n >= n_0 f(n) = k cal(L)(g(n))$ 
]

#definition(sub : "Big Oh (from hierarchy)")[
  $f(n) cal(O)(g(n)) (n -> oo) <=> f(n) prec.eq g(n)$
]

#proof(thm : [The above definations are equivalent.])[
  *Lemma:* Both the definitions are equivalent to $limsup_(n -> oo) (|f(n)|)/(|g(n)|) < oo$.

  From the first definiton, there exists an $k, n_0$ such that $forall n_0 <= n$:
  $
  f(n) = k cal(L)(g(n))\
  <=> |f(n)| <= k |g(n)|\
  <=> (|f(n)|)/(|g(n)|) <= (k)
  $ 
  As $n_0 < oo$ and $k < oo$; thus,
  $
  limsup_(n -> oo) (|f(n)|)/(|g(n)|) < oo
  $

  The equivalence from the second defination follows from the definition of asymptotic dominance.
]

We also allow this lemma to also be a defination of $cal(O)$.
#definition(sub : "Big Oh (limit)")[
  $f(n) cal(O)(g(n)) (n -> oo) <==> limsup_(n -> oo) (|f(n)|)/(|g(n)|) < oo$.
]

We also give the classical defination.
#definition(sub : "Big Oh (Classical)")[
  $f(n) = cal(O)(g(n)) (n -> oo) <==> exists c, n_0 op("s.t.") forall n >= n_0, |f(n)| <= c dot |g(n)|$
  ]

#proof(thm : [The above definations are equivalent.])[
  Left as excercise.
]

While we have used $limsup$ here, it is only to deal with the cases where the limit is not gurenteed to exist. We can use $lim$ in place for all the definations till here in most cases we will be concerned with.

In the family of big oh, we also have some more notations one must be aware of. This is called the Bachmann–Landau family, with contributions from Hardy and Knuth.
#definition(sub : "Bachman-Landau Notation")[
  #table(columns: 4, 
  [Notation], [Name], [Classical Definition], [Limit Definition],
  [$f(n) = Theta(g(n))$], [Big Theta], [$exists k_1, k_2, n op(s.t.) forall n > n_0, k_1 g(n) <= f(n) <= k_2 g(n)$], [$
  f(n) asymp g(n)
  $],
  [$f(n) = Omega(g(n))$], [Big Omega], [$exists k, n_0 op(s.t.) forall n >= n_0, f(n) >= k g(n)$], [$
  liminf_(n -> oo) (f(n))/(g(n)) = 0
  $],
  [$f(n) tilde g(n)$],[Total Asymptotic Equivalence (diffrent for asymptotic eqivalende defined using $asymp$.)],[],[$
  lim_(n -> oo) (f(n))/(g(n)) = 0
  $],
  [$f(n) = cal(o)(g(n))$], [Small Oh], [$exists k, n_0 op(s.t.) forall n >= n_0, f(n) <= k g(n)$], [$
  lim_(n -> oo) (f(n))/(g(n)) = 0
  $],
  [$f(n) = omega(g(n))$], [Small Omega], [$exists k, n_0 op(s.t.) forall n >= n_0, f(n) > k g(n)$], [$
  lim_(n -> oo) (f(n))/(g(n)) = oo
  $]
  )
]
#proof(thm: [If $f in F$ evivalence class and $hat(f)$ is the reprasentative of the class, then $f(x) = Theta(hat(f)(x))$])[
  Follows trivially from defination.
]
Let's play around with all this to see why we care so much about all this and speant all this time devloping this theory.



// #definition(sub : "Big Oh (from Hierarchy)")[
//   $f(x) = cal(O)(g(x))$ where $f, g$ are in the same equivalence class $F$.
// ]