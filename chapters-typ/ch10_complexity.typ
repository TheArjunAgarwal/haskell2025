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
  $f(x) = o (g(x)) (n -> oo) <==> exists k, n_0 op("s.t.") forall n >= n_0 f(n) = k cal(L)(g(n))$ 
]

#definition(sub : "Big Oh (from hierarchy)")[
  $f(n) o (g(n)) (n -> oo) <=> f(n) prec.eq g(n)$
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

We also allow this lemma to also be a defination of $o $.
#definition(sub : "Big Oh (limit)")[
  $f(n) o (g(n)) (n -> oo) <==> limsup_(n -> oo) (|f(n)|)/(|g(n)|) < oo$.
]

We also give the classical defination.
#definition(sub : "Big Oh (Classical)")[
  $f(n) = o (g(n)) (n -> oo) <==> exists c, n_0 op("s.t.") forall n >= n_0, |f(n)| <= c dot |g(n)|$
  ]

#proof(thm : [The above definations are equivalent.])[
  Left as excercise.
]

While we have used $limsup$ here, it is only to deal with the cases where the limit is not gurenteed to exist. We can use $lim$ in place for all the definations till here in most cases we will be concerned with.

In the family of big oh, we also have some more notations one must be aware of. This is called the Bachmann–Landau family, with contributions from Hardy and Knuth.
#definition(sub : "Bachman-Landau Notation")[
  #table(columns: 4, 
  [Notation], [Name], [Classical Definition], [Limit Definition],
  [$f(n) = Theta(g(n))$], [Big Theta], [$exists k_1, k_2, n op(s.t.) forall n > n_0, k_1 g(n) <= |f(n)| <= k_2 g(n)$], [$
  f(n) asymp g(n)
  $],
  [$f(n) = Omega(g(n))$], [Big Omega], [$exists k, n_0 op(s.t.) forall n >= n_0, |f(n)| >= k g(n)$], [$
  liminf_(n -> oo) (|f(n)|)/(g(n)) = 0
  $],
  [$f(n) tilde g(n)$],[Total Asymptotic Equivalence (diffrent for asymptotic eqivalende defined using $asymp$.)],[],[$
  lim_(n -> oo) (f(n))/(g(n)) = 1
  $],
  [$f(n) = o (g(n))$], [Small Oh], [$exists k, n_0 op(s.t.) forall n >= n_0, f(n) <= k g(n)$], [$
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

Here is an example to illustrate the diffrence between the notations.

- $3n^2 − 100n + 6 = o (n^2)$, because I choose $c = 3, n_0 = 6/100$;
- $3n^2 − 100n + 6 = o (n^3)$, because I choose $c = 1, n_0 = 7/100$;
− $3n^2 − 100n + 6 != o (n)$, because for any $c$ I can take $n > 100+ c$;

- $3n^2 − 100n + 6 = Omega(n^2)$, because I choose $c = 2, n_0 = 100$;
- $3n^2 − 100n + 6 != Omega(n^3)$, because for an $c$, I can take $n > 100/c$, if $c <1$ and $n > c$ otherwise;
− $3n^2 − 100n + 6 = Omega(n)$, because for any $c$, I can take $n_0 = 100 c$;

- $3n^2 − 100n + 6 = Theta(n^2)$, because both $o $ and $Omega$ apply;
- $3n^2 − 100n + 6 != Theta(n^3)$, because $Omega$ fails;
− $3n^2 − 100n + 6 != Theta(n)$, because $o $ fails.

Also note, $3n^2 - 100n + 6 tilde 3n^2$ by taking the limit.

Finally, note that $o , omega$ are equal to $o , Omega$ here as our limits exist.

#exercise(sub : "Big Oh Arithematic")[
  Prove the following:
  + $n^m = o (n^(m')) quad m <= m'$
  + $o (f(n)) + o (g(n)) = o (|f(n)| + |g(n)|)$
  + $f(n) = o (f(n))$
  + $o (o (f(n))) = o (f(n))$
  + $c o (f(n))= o (f(n))$ where $c$ is constent
  + $o (f(n))o (g(n)) = o (f(n)g(n)) = f(n)o (g(n)) = g(n)o (f(n))$
  + $o (f(n)^2) = o (f(n))^2$
]


= Asymptotic Mathematics
Notice that Hardy was involoved in devloping some of this notation. What has an analytical number theory got to do with a tool for algorithmic analysis?

We wrote $n -> oo$ next to $o $ in the definitions as we are consider the limits approaching $oo$. We can also define similerly for $n -> 0$ and $n -> k$. These definitions are more common in math than CS, as mathematicains care about behavior of functions at a lot of places other than $oo$. As this is a CS book, in absence of clarification, assume $n -> oo$.

This allows us to write taylor series in big-oh notation, for example as $sin(x) = x - x^3/3! + o (x^5) (x -> 0)$.

All this can be made useful in problems like: 

#exercise(sub : "Sum of Power of Numbers")[
  Comment on the asymptotic behavior of $
  f_k(n) = sum_(i=1)^n i^k
  $ where $k, n in NN$. 
]
Let's start with some small cases.
$
f_0(n) = sum_(i=1)^n i^0 =sum_(i=1)^n 1 = n = o (n)\
f_1(n) = sum_(i=1)^n i^1 =sum_(i=1)^n i = (n(n+1))/2 = o (n^2)\
f_2(n) = sum_(i=1)^n i^2 = (n(n+1)(2n+1))/6 = o (n^3)
$
This seems to indicate $f_k(n) = o (n^(k+1))$, but how do we prove this?

#definition(sub : "Derivative with big Oh")[
  We can define the derivative of $f$ through this equation#footnote[This only works if $f$ has a strong or strict derivative. For most functions we care about, this is true (and equals their usual derivative). This note is included to clarify in case you have doubts, despite the fact we do not wish to go into the further technicalities of this.]
  $
  f(x+h) = f(x) + h f'(x) + o (h^2) quad (h -> 0)
  $
]
#definition(sub : "Binoial Exapansion with Big Oh")[
  $
  (1+x)^n = 1 + n x + binom(n,2) x^2 + dots + binom(n, k) x^k + o (x^(k+1)) quad (n -> 0)
  $
]
#definition(sub : "Taylor Series with Big Oh")[
  $
  f(x) = f(0) + f'(0) x + f''(0) x^2/2! + dots + f^((i))(0) x^(i)/i! + cal(O)(x^(i+1)) quad (x -> 0)
  $
]

This allows us to define $(x+h)^(k+1) = x^(k+1) + h(k+1)x^k + o (h^2)$, taking $h = 1$;
$
(x+1)^(k+1) = (x)^(k+1) + (k+1) x^k + o (1)\
(x+1)^(k+1) - (x)^(k+1) = (k+1) x^k + o (1)\
sum_(x = 0)^(n) [(x+1)^(k+1) - (x)^(k+1)] = sum_(x = 0)^(n) [(k+1) x^k + o (1)]\
sum_(i = 1)^(n+1) [i^(k+1) - (i-1)^(k+1)] = (k+1) sum_(x = 1)^(n) x^k + o (n)\
((n+1)^(k+1) - o (n))/(k+1) = sum_(x = 1)^(n) x^k\
sum_(x = 1)^(n) x^k = (n^(k+1) + (k+1)n^k + o (1) - o (n))/(k+1)\
sum_(x = 1)^(n) x^k = o (n^(k+1))
$

We can also use this notion to reprasent some mathematics results. For example:
#definition(sub : "Prime Number Theorem")[
  Let $pi(n)$ reprasent the number of primes less then $n$. Then:
  $
  pi(n) tilde n/(ln(n))
  $
]
#definition(sub : "Stirling Approximation")[
  $
  n! = sqrt(2 pi n) (n/e)^n (1+ 1/(12n) + o (1/n^2))
  $ #footnote[More terms can be obtained by using the Bernolli numbers. We have gone for this form as the proof follows from probability theory and doesn't involve results from complex analysis.]
  Also note,
  $
  n! tilde  sqrt(2 pi n) (n/e)^n\
  => n! = Theta(sqrt(n) (n/e)^n)
  $
]
#definition(sub : "Binomial Coefficient")[
  When $k$ is fixed,
  $
  binom(n,k) tilde n^k/k! = Theta(n^k)
  $

  When $n, k$ are both large, we can use the Stirling Approximation to get:
  $
  binom(n,k) tilde sqrt((n)/(2 pi k(n-k))) ((n^n)/(k^k (n-k)^(n-k)))  
  $
]
One case we have suspiciously left out is when $k$ is small.
#exercise(sub : "Small k binomials")[
  We will restrict ourselves to $k = o (n)$ as other cases get messy rather quickly.

  Notice
  $
  binom(n, k) = A(n, k) n^k/k!
  $
  where
  $
  A(n, k) := product_(i=1)^(k-1) (1- i/n)
  $
  Thus, to find the asymptotics, we just need to solve for $A$.
  $
  ln(A(n,k)) = sum_(i=1)^(k-1) ln(1-i/n)
  $

  Solve when

  (i) $k = o (sqrt(n))$

  (ii) $k = c sqrt(n)$ and $c$ is known

  (iii) $k = o (n^(2/3))$

  (iv) $k = o (n^(3/4))$
  
  (v) $k = o(n)$
]
We will solve the first 3 cases and leave the rest for you, the reader.

(i) Using @definition_of_Taylor_Series_with_Big_Oh on $ln(1+x) = x + o(x^2)$, we will get:
$
ln(A(n,k)) &= sum_(i=1)^(k-1) ln(1-i/n)\
&= - sum_(i=1)^(k-1) (i/n + cal(O)(i/n)^2) quad &(i/n -> 0)\
 &tilde - k^2/(2n) + cal(O)(k^3 n^(-2))  quad &(k/n -> 0) quad &"using the sum of consecutive terms" \
 &= o(1) quad  &(k, n -> oo) quad &"using" k = o(sqrt(n))\
$
This implies $A = e^(o(1)) = o(1) => binom(n, k) = o(1) dot n^k/k! = o(n^k/k!)$.

(ii) Here the $c$ being specified gives us $A = e^(-c^2/2) => binom(n,k) = e^(-c^2/2) n^k/k!$.

*Hint for (iii), (iv), (v):* We were able to disolve the $cal(O)(k^3 n^(-2))$ term as if $k = o(sqrt(n))$, the term would be $cal(O)(1/sqrt(k))$ and goes to zero, rapidly.

We can 'fix' this by having a slightly bigger $k = o(n^(2/3))$. The same happens in *(iv)* as we take a more and more accurate Taylor exapansion for $ln(1-i/n)$. Can we get some sort of a limiting case and solve for $k = o(n)$?

#exercise(sub : "Bootstrap(Erdos, Greene)")[
  Find the asymptotics of $f$ satisfying:
  $
  f(t) e^(f(t)) = t 
  $
  as $t -> oo$
]
The idea here is to begin with a weak bound and progressivle make it stronger.

Here we start by simplyfying the equation:
$
ln(f(t)) + f(t) = ln(t) quad (t -> oo)\
$
We know that $f(x) succ ln(f(x))$, thus $
lim_(t -> oo) (ln(f(t)) + f(t))/ln(t) = 1\
=> lim_(t -> oo) f(t)/ln(t) = 1\
=> f(t) = Theta(ln(t))\
$
Making this substitution gives us,
$
f(t) = ln(t) - Theta(ln(ln(t)))
$
Now, making this substitution gives us,
$
ln(ln(t) - Theta(ln(ln(t)))) + f(t) = ln(t)\
ln(ln(t) ( 1 - Theta((ln(ln(t)))/(ln(t))))) + f(t) = ln(t)\
ln(ln(t)) + ln(1 - Theta((ln(ln(t)))/(ln(t)))) + f(t) = ln(t)\
f(t) = ln(t) - ln(ln(t)) + Theta((ln(ln(t)))/(ln(t)))
$
And one can continue getting more and more terms by this process. We stop here as getting the logitherm now will be much harder.

#exercise(sub : "A Weird Condition")[
  Prove that there are infinitely many integers $n$ such that the sum of digits of $n$ in base $p < n$ exceeds $2025$ for every prime p.
]

This question is incredibly hard to answer through regular means of construction of contradiction. 

We will try to set up an asymptotic bound on number of integers violating the condition and show that their density is less than $1$, hence, showing that the condition is true for infinitly many numbers.

Let's say $x < N$ integrs violate these condition for some large $N$. Notice,
$
forall "primes" p <= n, exists k in NN op(s.t.) n^(1/(k+1)) < p <= n^(1/k)
$

Thus, given a $p$, any number from $1..N$ has atmost $k+1$ digits in base $p$. Thus, a violation can only occcur if the sum of digits: $d_1, d_2, dots, d_(k+1) < p$ is less than $2025$.

We can (grossly) overcount the number of violaters using the sum:
$
x < sum_(k) sum_(N^(1/(k+1)) < p <= N^(1/k) \ p "is prime") binom(k+2026, 2025)\
$

To might be as good time as any to get the bounds on $k$.
$
pi(N^(1/k_m)) - pi(N^(1/(k_m+1))) = 0\
(k_m N^(1/k_m))/(ln(n)) - ((k_m+1) N^(1/(k_m+1)))/(ln(n)) = 0 \
k_m N^(1/k_m) = (k_m+1) N^(1/(k_m+1)) \
N^(1/(k_m (k_m+1))) = 1 + 1/k_m \
1/(k_m (k_m+1)) ln(N) = ln( 1 + 1/k_m)\
1/(k_m (k_m+1)) ln(N) = 1/k_m + O(1/k_m^2)\
1/(k_m+1) ln(N) = 1 + O(1/k)\
ln(N) = k + O(1) + O(1/k_m)\
k_m = Theta(ln(N))
$

Notice, for $k = 1$,
$
sum_(sqrt(N) < p <= N\ p "is prime") binom(2027, 2025) =  cal(O)(pi(N)) < N/3
$
And for $k > 1$,
$
sum_(k>1)^(k_m) sum_(N^(1/(k+1)) < p <= N^(1/k) \ p "is prime") binom(k+2026, 2025)\
= sum_(k>1)^(k_m) sqrt(N) Theta(k^2025)\
= sqrt(N) Theta(ln(N)^2025) Theta(ln(n))\
=sqrt(N) Theta(ln(N)^2026)\
<  N/3
$

Thus, $x < (2N)/3$ for some $n$. Thus, atleast $N/3$  integers exist satisfying our property for some large $N$.

= Analysis of Algoritms
With the (damn scary) math out of the way, we will now move to algoritms and the real reason we are concerned with asymptotics. Take a deep breath, relax; the scary part is beyond us.






// = Exercise
// #exercise(sub : "A strange pair?")[
//   Can you find a pair of functions such that $f(n) != o (g(n))$ and $f(n) != Omega(g(n))$?

//   What if they both had to be monotonic increasing?
// ]


// At some point, I'll fix the absolute values in some fractions. As we fix g ourselves, we neednot have absolute there in some defninitions. 