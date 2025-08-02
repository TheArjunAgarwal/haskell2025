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

Before we get to designing and analyzing algorithms, let's pause and breifly question what ‘algorithm’ actually means. To quote Hannah Fry,

#quote(sub : "Hannah Fry")[
It’s a term that, although used frequently,
routinely fails to convey much actual information. This is partly because the word itself is quite vague. Officially, it is defined as follows:

_algorithm (noun): A step-by-step procedure for solving a problem or accomplishing some end especially by a computer._

An algorithm is simply a series of logical instructions that show, from start to finish, how to accomplish a task. By this broad definition, a cake recipe counts as an algorithm. So does a list of directions you might give to a lost stranger. IKEA manuals, YouTube troubleshooting videos, even self-help books – in theory, any self-contained list of instructions for achieving a specific, defined objective could be described as an algorithm. But that’s not quite how the term is used. Usually, algorithms refer to something a little more specific. They still boil down to a list of step-by-step instructions, but these algorithms are almost always mathematical objects. They take a sequence of mathematical operations – using equations, arithmetic, algebra, calculus, logic and probability – and translate them into computer code. They are fed with data from the real world, given an objective and set to work crunching through the calculations to achieve their aim. They are what makes computer science an actual science, and in the process have fuelled many of the most miraculous modern achievements made by machines.
]

We will take Prof. Fry's defination as the gospel as trying to go into more details will open up questions which are more philosophical than we wish to be here.

== Sorting
A classic example in algorithm design is sorting. We have already seen some sorting algorithms, but here we will try to do an complete analysis of them.

There are two clear naive algorithms one can think about:
- We can repeatedly move through the list element by element, comparing the current element with the one after it, swapping their values if needed. Do these passes through the list are repeated until no swaps have to be performed in a pass, meaning that the list has become fully sorted. This is called Bubble Sort.
- We can take the first element as sorted list and then keep inserting the succesive elements in a way that mantains the sorted property. This is called Insertion sort.

We can implement these ideas as:
```
bubble [] = ([], False)
bubble [x] = ([x], False)
bubble (x:y:ys)
    | x > y = (y : fst (bubble (x:ys)), True)
    | otherwise = (x : fst (bubble (y:ys)), snd (bubble (y:ys)))
bubblesort xs = let (a,b) = bubble xs in if b then bubblesort a else a

insert x [] = [x]
insert y (x:xs) = if x > y then y:x:xs else x : (insert y xs)

insertSort :: Ord a => [a] -> [a]
insertSort = foldr insert []
```

Bubble Sort is bsically keeping the last element to its place every pass. So the first pass takes $n$ comparisons and in the worst case, $n-1$ switches. Similerly, the second pass takes $n$ comparisons and at most $n-2$ switches.

In the worst case, we will need to make $n$ passes (the last one to check if the list is sorted. Thus, $n^2$ comparisons and $((n-1)n)/2$ switches.

This is a very weird way to express the speed of the algorithm but the time to compare and switch two elements is diffrent. So what do do? Simple, use the fact that they are constent and take them as $cal(O)(1)$. This assumption also makes our analysis easier by letting us remove some small factors and focus on large improvements.

Thus, bubblesort is $cal(O)(n^2)$.

Similerly, `insert` takes $cal(O)(k)$ time in the worst case to insert an element in a list of length $k$. Thus, `insertSort` takes $cal(O)(1) + dots + cal(O)(n) = cal(O)(n)$ time. (notice, we didn't talk about comparsions and switches as the big Oh allows us to think more simplistically).

The algorithms for sorting we saw in chapter 7 wer merge sort and quick sort which worked as follows:
```
merge :: Ord a => [a] -> [a] -> [a]
   merge [] [] = []
   merge [] ys = ys
   merge xs [] = xs
   merge (x:xs) (y:ys) = if x < y
     then x : merge xs (y:ys)
     else y : merge (x:xs) ys


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = quickSort [l | l <- xs, l > x] ++ [x] ++ quickSort [r | r
<- xs, r <= x]
```

We had shown, through an cumbersome computation, that `mergesort` takes less than $n log(n) + 3n + 1/2 log(n) + 1/2$ compatsions. Doing so with switches seems like a much sadder condition to be in. 

But with the power of Big-Oh on our side, we just assume the worst case where every comparsion leads to a switch and get the number of operations as $cal(O)(n log(n))$.

Similerly, we can use the results from there, to shown that the worst case for `quicksort` is $cal(O)(n^2)$ operations, but in the average case, it only takes `cal(O)(n log(n))` operations.

But can we do better? Depends on how much you know about the elements of the list.

Say you have a bunch of sphegatti and you want to sort them. Well, just level them across the table. Then move your hand from top towards the tops of the sphegatti till you hand touches the tallest sphegatti, pull it out and continue. This is a $cal(O)(n)$ algorithm. It even has a name Sphegatti Sort.

But let's say I have to sort a bunch of spegatti sauces according to how much a distingushed person likes them and I can only ask them to compare two sauces at a time. In this case, we can prove that we can't do better than $cal(O)(n log(n))$.
#proof(thm: [An Sorting algorothm when we can’t access the items’ values directly: only compare two items and find out which is bigger or smaller can't be better than $cal(O)(n log n)$ in the worst case.
])[
  In an $n$ element list, knowing that $a_1 > a_2$ reduces the possible sortings from $n!$ to $n!/2$.

  Similerly, knowing any more comparisions will reduce the number of possible orders by $2$.

  Thus, we need atleast $ceil(log(n!))$ comparisons.

  We claim $ceil(log(n!)) = cal(O)(n log(n))$. This is true as:
  $
  ceil(log(n!))\
  = ceil(log(n^n)) quad "as " n^n > n!\
  = ceil(n log(n))\
  = cal(O(n log(n))) quad "as " f(x) - 1 <= ceil(f(x)) <= f(x)+1
  $
]

This bound holds for randomized algorithms as well (like say a quickSort picking random pivots). The theorem is that no randomized algorithm can do better than $cal(O)(n log n)$ operations in the average case. 

While proving so formally will require us to define some terms like probability distribution and bit tapes, here is the basic idea.

Given a list with infinite random numbers, our randomized algorithm uses these to do it's randomizition. Thus, given this list, we have a normal non-random algorithm. Thus, we can now find the expectation and get that it has the same lower bound of $cal(O)(n log n)$.

Such proofs help us make sure we have achived optimality and make sure people don't waste time looking for faster algorithms. However, we just sorted spegatti in $cal(O)(n)$ time, how?

The answer 'that was a stupid algorithm' is plain wrong as our proof never relied on dumbness or smartness. So what is at play?








= What Big O doesn't want you to know?



== Galactic Algorithms
A galactic algorithm is one with an optimal theoretical asymptotic performance, but which is never used in practice. Typical reasons are that the performance gains only appear for problems that are so large they never occur, or the algorithm's complexity outweighs a relatively small gain in performance. Galactic algorithms were so named by Richard Lipton and Ken Regan,[1] because they will never be used on any data sets on Earth.

= An Informal Survey of Multiplication Algorithms
The word Algorithm orignates from French where it was the mistranslated name of the 9th Century Arabic scholer Al-Khwarizmi, who was born in present day Uzbekistan, who studied and worked in Baghdad. His text on multiplying indo-arabic numerals travelled to Europe and his name was mis translated to "Algorisme" which later evolved into algorithm. While we will see other algorithms of the ancients, let's begin with the OG multiplication. We will consider the multiplication of two $n$ digit numbers, given it takes a single operation to solve for $n=1$ (base cases). We assume additions of sigle digit takes $cal(O)(1)$ (constent) time, and hence, adding a $m, n$ digit number takes $cal(O)(min(m,n))$ time. Similerly, if a function calls some other function, we say this call takes $cal(O)(1)$ time.

The naive way to do so would be to define multiplication as repeated addition. Something of the sort `multiply 1 b = b` and the reccurence. `multiply a b = b + multiply (a-1) b`. For two `n` digit numbers, this will take $cal(O)(10^n) dot cal(O)(n) = cal(O)(n 10^n)$ operations as $b < 10^n$ and we need to make $b$ function calls and as many additions with the smallest number of size $n$. That is very bad, we will see the quantitatatives in a moment. #footnote[
  We obviously know that the function definition is not complete. We need to deal with negitives and zero, but all of that doesn't really change the time complexity.
]

Notice, this is a departure from our usual method of counting every operation and we are instead taking the big-oh approximation. We will talk why this is a good idea most of the times in some while, but in this case, it is the only way to deal with the mix of operations we are using and reconciling number of operations.

An improvement in the multiplication algorithm, we are already familier with, is the one taught in school. This was also the algorithm Al-Khwarizmi found. The number of operations fort this algorithm is $cal(O)(n^2) + cal(O)(n) cal(O)(n-1) = cal(O)(n)$ as we multiply all the digits in the latter number with the digits in the former number and then add the results suitably one by one. That is in $32 * 45$, we will compute $32 * 5$ and $32 * 4$ and add them.

Doing a lot better than this took about a millenia, with the improvement comming from Anatoly Karatsuba in 1962. The idea used is the usual divide and conquor.

We divide the fist number into $x = a*10^(n/2) + b$ and the second number as $y = c*10^(n/2) + d$. Thus,
$
x y = (a*10^(n/2) + b) * (c*10^(n/2) + d)\
= a c * 10^n + b c * 10^(n/2) + a d * 10^(n/2) + b d
$

Let's say it take $T(n)$ operations to multiply two $n$ digit numbers. Thus, our problem of multiplying two $n$ digit numbers can be reduced to multiplying two $n/2$ digit number $4$ times. 

$
T(n) = 4 T(n/2) + cal(O)(n) + cal(O)(1)\
=> T(n/2) = 4T(n/4) + cal(O)(n/2) + O(1)\
=> dots
=> T(n) = 4^(log(n)) + cal(O)(n) cal(1) * log(n)\
=> T(n) = n^2 + cal(O)(log(n)) = cal(O)(n^2) 
$

But we didn't do any better! All the effort seems to be in vain. Well, this is where Karatsuba's brillience comes to play.
$
x y = a c * 10^n + b c * 10^(n/2) + a d * 10^(n/2) + b d\
x y = a c * 10^n + (b c + a d) * 10^(n/2) + b d\
$

We only need three terms. If we could somehow figure out $b c + a d$ without needing to find them both individually. The genius idea was:
$
(a+b)(c+d) = a c + b d + (b c + a d)
$
and we already have computed $a c$ and $b d$. If we subtract them, we will have the third term. Notice, $a + b, c+d$ are atmost an $n/2+1$ digit numbers. Thus, we can now only make three smaller multiplications. We will claim $T(n + 1) = T(n) + cal(O)(n)$ as we can divide the given $n+1$ digit numbers into two parts, the most significent $n$ digits and then the last digit and complete the computation.

This will give us,

$
T(n) = 3 T(n/2) + cal(O)(n) + cal(O)(1)\
=> T(n) = 3^(log(n)) + cal(O)(n) + cal(O)(1) log(n)\
=> T(n) = cal(O)(n^(log(3))) approx cal(O)(n^(1.6))
$

This is a lot better. The next improvement came just an year later in 1963 by Tooom and Cook, making it $cal(O)(n^(log_3(5)))$. Here is what we belive their research process looked like:
#figure(
  image("../images/multiplication-algo-meme.png", height: 50%))

If you are wondering, they showed that we can break the multiplication in five $n/3$ sized products. Actually, we can split in any number of parts we want. Karatsuba is Toom-2, the $cal(O)(n^(log_3(5)))$ algorithm is Toom-3.  When split in some $k$ parts, the complexity is $cal(O)(n^E)$ where $E = log_k(2k − 1))$.

This can in theory do $cal(O)(1)$ multiplication. As we will see in the upcoming section on the dark secrets of big-oh, $cal(O)$ sweeps the constents under the rug. This works when the rug is heavy, large and the constents tiny. But if, the constent is large: well then $Theta(10^(10^100) n)$ is worse than $ Theta(n^2)$ for all values we could care about, irrespective of the fact the former is $cal(O)(n)$ while the latter is $cal(O)(n^2)$.

Doing an exact complexity analysis can allow us to compute the exact speed of growth of the constent of $cal(O)(n^E)$ (hint: It is basically exponential).

This leads us to the $cal(O)(n log(n) log(log(n)))$ Schönhage–Strassen algorithm (1971) which uses the Discrete Fast Fouries Transform algorithm described in chapter 8. The exact implementation is left as excercise (to find and understand) to the morbidly curious. In this paper, Arnold Schönhage and Volker Strassen also conjectured a lower bound of $Omega(n log(n))$.

This is about the end of multiplication algorithms I can hope to talk about with the material in this book. Also, the constents hidden by big-oh become so large that most implementations use Karatsuba or Toom-3 till some size and then switch to Schönhage–Strassen. So everything here onwards are just fun facts. 

The next leap came in 2007, when Martin Fürer improved the bound to $cal(O)(n log(n) 2^(cal(O)(log^*(n))))$ where the $log^*(n)$ denotes the number of times we must take $log(n)$ before we go below $1$. This leap was made possible due to half-DFT's, lots of ring theory and complex analysis and certain results about primes of form $p = 2^(2^k) + 1$ turining up true. This algorithm beats Schönhage–Strassen for integers with about $10^19$ digits.

The next improvement came by using number theory inplace of complex analysis and other changes courtasy Anindya De, Piyush P Kurur, Chandan Saha and Ramprasad Saptharishi #footnote[] (2008) which beats this Fürer for numbers with about $10^4796$ digits.

In 2015, David Harvey, Joris van der Hoeven and Grégoire Lecerf gave a new algorithm which replaced the $cal(O)(log^*(n))$ with $3 log^*(n)$. The only issue is that this paper used certain unproven conjuctures on Mersenne primes. 

In 2015-16, in a series of two papers, Svyatoslav Covanov and Emmanuel Thomé first made a new algorithm with same complexity as Fürer and then, using unproven conjuctures on Fermat Primes and genralizations, produced an algorithm where $cal(O)(log^*(n))$ is replaced with $2 log^*(n)$.

Not to be defeated so easily, Harvey and Hoevan snapped back in 2018 with an algorithm which acives the $2 log^*(n)$ complexity without any conjuctures. They use Minkowski's theorem (which funnily was proven in 1889).

And to end this on a win, Harvey and Hoeveen publish the first $cal(O)(n log(n))$ in March 2019.

#quote(sub : "David Harvey and Joris van der Hoeven")[...our work is expected to be the end of the road for this problem, although we don't know yet how to prove this rigorously.”]

So well, can we do better is still an open question.

Anyways, for this paper, they shared the de Bruijn medal in 2022. Well, we have come full circle I guess.


// = Exercise
// #exercise(sub : "A strange pair?")[
//   Can you find a pair of functions such that $f(n) != o (g(n))$ and $f(n) != Omega(g(n))$?

//   What if they both had to be monotonic increasing?
// ]


// At some point, I'll fix the absolute values in some fractions. As we fix g ourselves, we neednot have absolute there in some defninitions. 