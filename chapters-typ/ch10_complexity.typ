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

Similerly, we can use the results from there, to shown that the worst case for `quicksort` is $cal(O)(n^2)$ operations, but in the average case, it only takes $cal(O)(n log(n))$ operations.

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
  = cal(O)(n log(n))) quad "as " f(x) - 1 <= ceil(f(x)) <= f(x)+1
  $
]

This bound holds for randomized algorithms as well (like say a quickSort picking random pivots). The theorem is that no randomized algorithm can do better than $cal(O)(n log n)$ operations in the average case. 

While proving so formally will require us to define some terms like probability distribution and bit tapes, here is the basic idea.

Given a list with infinite random numbers, our randomized algorithm uses these to do it's randomizition. Thus, given this list, we have a normal non-random algorithm. Thus, we can now find the expectation and get that it has the same lower bound of $cal(O)(n log n)$.

Such proofs help us make sure we have achived optimality and make sure people don't waste time looking for faster algorithms. However, we just sorted spegatti in $cal(O)(n)$ time, how?

The answer 'that was a stupid algorithm' is plain wrong as our proof never relied on dumbness or smartness. So what is at play?

As stated above, if we know more about what we are sorting, we could do better. For example, if they were sphegatti...

Given that most sorting is of integers and strings, why would you ever so restrict yourself as to only use comparisons? You can do much more with these objects that. You can add them, you can multiply them, you can count with them!

While it may seem unintutive, but we can use even these operations for sorting! We will talk mostly about integer sorting algorithms but faster algorithms for strings also exists (which we will mention).

=== Counting Sort
Let's say we want to sort a range of $0-k$ with some number of repeats. So what do we do? We can do this in $cal(O)(n k)$ time.#footnote[
  If we had an array, we could do this in $cal(O)(n+k)$ time as we have random access. 
]

```
-- | Counting Sort
countingSort :: Int -> [Int] -> [Int]
countingSort k lis = concat (go k lis []) where
    go (-1) _ ans = ans
    go k lis ans = go (k-1) lis (filter (==k) lis : ans)
```

For a fixed $k$, this is already an $cal(O)(n)$ algorithm; but it is easy to observe, that in practice this would behave $cal(O)(n^2)$-ish.

And secondly, if we want to fix a large $k$, it better be `maxBound :: Int` atleast. That makes the algorithm insanly slow while still being $cal(O)(n)$.

So why talk about it? Because it is a subroutine to 
=== Radix Sort
We begin by first making modefications to `countingSortWithKey :: Int -> [(Int, Int)] -> [(Int, Int)]` which will sort based on the key value of the pairs (the second value of the pair) keeping the list otherwise stable (we don't change the order from that prescribed by orignal list if key value is same).
```
-- | Counting Sort With Keys
countingSortWithKey :: Int -> [(Int, Int)] -> [(Int, Int)]
countingSortWithKey k lis = concat (go k lis []) where
    go (-1) _ ans = ans
    go k lis ans = go (k-1) lis (filter (\y -> k == snd y) lis : ans) -- The single change!
```

#definition(sub : "Radix Sort")[
  Radix sort is a sorting algorithm which sorts a list of integers digit by digit, starting from the least significent digit; mantaining stablity in subsequent sorts.
]
For example, if we had to sort:
$
853, 872, 265, 238, 199, 772, 584, 204, 480, 173,\
499, 349, 308, 314, 317, 186, 825, 398, 899, 161
$
By the described process, We would first sort using the one's digit.
$
48bold(0), 16bold(1), 87bold(2), 77bold(2), 85bold(3), 17bold(3), 58bold(4), 20bold(4), 31bold(4), 26bold(5),\
82bold(5), 18bold(6), 31bold(7), 23bold(8), 30bold(8), 39bold(8), 19bold(9), 49bold(9), 34bold(9), 89bold(9)
$
We will now sort using the ten's digit, remember, we need to be stable that is for numbers that are tied on the middle digit, keep them in the current order.
$
2bold(0)underline(4), 3bold(0)underline(8), 3bold(1)underline(4), 3bold(1)underline(7), 8bold(2)5, 2bold(3)8, 3bold(4)9, 8bold(5)3, 1bold(6)underline(1), 2bold(6)underline(5),\
8bold(7)underline(2), 7bold(7)underline(2), 1bold(7)underline(3), 4bold(8)underline(0), 5bold(8)underline(4), 1bold(8)underline(6), 3bold(9)underline(8), 1bold(9)underline(9), 4bold(9)underline(9), 8bold(9)underline(9)
$
Finally, we will sort using the hundered's number.
$
161, 173, 186, 199, 204, 238, 265, 308, 314, 317,\
349, 398, 480, 499, 584, 772, 825, 853, 872, 899
$

#exercise(sub:"Proof of Correctness")[
  Show that Radix Sort correctly sorts an input list of $n$ integers via induction of the length of longest number in the list.

  Hint: You might want an induction hypothesis which looks more like the process. Something along the lines that the last $j$ places are sorted in $j$th pass.
]
So how do we quickly sort the numbers by the last places? Use it as a key and use `countingSortWithKey`. This would look like:
```
digit pos y = (y `mod` (10 ^ (pos + 1))) `div` (10 ^ pos)

radixSort :: Int -> [Int] -> [Int]
radixSort maxLength lis = go 0 lis where
  go pos lis = if pos == maxLength then lis else go (pos + 1) newLis where
    key = map (digit pos) lis
    lisWithKey = zip lis key
    newLis = map fst $ countingSortWithKey 9 lisWithKey
```

This is a rather nice implementation, of what can easily be a very cumbersome code. A lot of helpers are created for more clarity instead of going for a more concise but impenetratable version.

Let's now compute the time complexity. We will take the number of digits in the base system to be $d$, the longest number be $l$ long and the list having $n$ numbers. 

Thus, every pass through the list takes:
- $Theta(n)$ time to get the digits.
- $Theta(n)$ time to zip the key and numbers up.
- $Theta(n d)$ time to counting sort with the given key.
- $Theta(n)$ time to map `fst`.

This will be a total of $Theta(n d)$ time. We will make $l$ passes through the list.

Thus, the total time complexity is $Theta(n d l)$#footnote[
  Again, as the time complexity for counting sort is different for arrays, the complexity of radix sort would also change.
]. Setting $d = 9$, we would have $Theta(n log_d (M)) = Theta(n log(M))$ which would make it asymptotically faster for any list with the largest number less than number of elements.

#exercise(sub : "Complete Radix Sort")[
  Modify the radix sort algortithm to make functions:
  ```
  radixSortWithBase :: Int -> Int [Int] -> [Int]
  -- Radix sort with some other base.
  radixSortPack :: [Int] -> [Int]
  -- The packaged radixSort which just takes a list of ints and sorts it, hidding the bells and whistles.
  ```
]

=== Survey of Sorting Algorithms
In this survey, we will consider the optimal data structures. Design and Analysis of (some of) them is dicussed in ch11.

We will let $w = log(M)$
#table(
  columns:4,
  [Published], [Algorithm / Authors], [Data Structure], [Complexity],
  [Since Antiquity], [Merge Sort], [List], [$cal(O)(n log(n))$],
  [Since Antiquity], [Radix Sort], [Arrey, List], [$cal(O)(n w/log(n))$ and for list, $cal(O)(n w)$],
  [1974], [van Emde Boas], [van Emde Boas Tree], [$cal(O)(n log(w/(log n)))$],
  [1983],[Kirkpatrick, Reisch],[Trie],[$cal(O)(n + w/log(n))$],
  [1995],[Andersson, Hagerup, Nilsson, Raman (called Signature Sort)],[Compressed Trie],[$cal(O)(n log log n)$ for $log^(2+epsilon)(w) >  n$],
  [2002],[Han, Throup],[Too Weird],[$cal(O)(n sqrt(log log n))$ for some nice bound on $w$)#footnote[
  The reason we don't describe Han-Throup Algorithm well as none of us are that very intrested in sorting algorithms and hence, don't have the level of knowledge of tree structures and algorithms needed to do a description of this justice.
]]
)

An open question is if it is possible to do sorting in $cal(O)(n)$. For example, if $w = Omega (log(n)) => "Radix Sort is" cal(O)(n)$.

What about smaller $w$? This was given by Andersson et. al. where $cal(O)(n)$ is achived for $w = cal(O)(n^(1/2 - epsilon))$.#footnote[One can also see the complexity when $log^(2+epsilon) w > n$ in the table. The middle cases are where a complex complexity (pun intended) form with $w$ and $n$ can be obtained.]

Belazzougui et. al. in 2014 gave a way to sort in $cal(O)(n)$ for $w = Omega(log^2(n) log log n)$. Their algorithm, called Packed Sort, works normally close to $cal(O)(n log n)$ but in certain cases becomes much faster.

A general proof or a single algorithm across $w$ is not yet known and not much progress has been made in the last decade. Same is true for randomized algorithms like quick Sort; while results are slightly better their, progress has slowed down considerably.

= RAM Model and Asymptotic Analysis
A change in our approach in this chapter is the fact that we never told you what time algorithms took to run for us. 

Radix sort was quite fast for whenever I ran it in my GHCI, it performed better than merge sort; but you needed neither my computer times nor took my word for it.

The thing is computers become more powerful all the time. An empirical relation which has been true upto recently is:

#quote(sub : "Moore's Law")[
The number of transistors in an integrated circuit (IC) doubles about every two years.#footnote[- It is an observation, not a law.
- It is argued to be slowing down, by some groups.]
]

So sometime in future, a mergesort on your computer would beat radix sort on mine. I need to argue that the radix sort will beat merge sort on your device; without seeing your device or the computer's proccessing paradigm then. Furthermore, algorithms almost always are faster in a low-level language like C or Assembly; and an algorithm is almost always slower in a high-level language like JavaScript or Python. Even worse, runtimes depend on how the compiler is optimizing your code, yes, some compilers do that.

With so many considerations, evaluating algorithms is quite hard.

This is quite a task and many methods have been proposed for a machine independent, language independent algorithm design. The first is to use a hypothetical machine called Random Access Machine or RAM (not to be confused with Random Access Memory which is a part of mordern computer architecture).
#definition(sub:"Random Access Machine")[
  Under this model of computation, we are confronted with a computer where:
- Each simple operation ($+, *, –, =,$ `if` and calling a function) takes exactly one time step.
- The called functions are not considered simple operations. Instead, they are the composition of many single-step operations. For example, if an algorithm calls `sort`, it should certainly take more than 1 time step.
- Each memory access takes exactly one time step. Further, we have as much memory as we need. The RAM model takes no notice of whether an item is in cache or on the disk.

Under the RAM model, we measure run time by counting up the number of steps an algorithm takes on a given problem instance.
]

The RAM is a simple model of how computers perform. Perhaps too simple. After all, multiplying two numbers takes more time than adding two numbers on most processors (and also is basically a sub-routine as we will see at the end of this chapter); violating the first assumption of the model. 

The presence of multiple proccessors and threads, which fancy compilers use, may as well violate the second assumption as we can be running the subroutine and routine at once. 

And certainly memory access times differ greatly depending on whether data sits in cache or on the disk. Finally, we don't have $oo$ memory.

This makes the model wrong on every single front. But here is a thing, it is still a usefull model.

Consider the model that light travels in straight lines. From a physics standpoint, we know this is fundamentally incorrect. Light exhibits wave properties, can be bent by gravity, and behaves according to complex electromagnetic principles. 

But when designing the lighting for a theater stage, the straight-line model of light is not only sufficient but essential. A lighting designer doesn't need to consider quantum mechanics or gravitational lensing when positioning spotlights; they simply need to predict where shadows will fall and how bright different areas will be. The straight-line model makes these calculations trivial and intuitive. 

However, when that same lighting designer needs to design a laser show with mirrors, they might need the more sophisticated model that accounts for reflection and refraction. And if they were designing equipment for their universities astronomy lab (explains the need for parttime working in theater) studying distant galaxies, they'd need to consider how gravity bends light around massive objects. Each level of complexity serves its purpose within the appropriate domain.

This is the case with RAM as well. We use RAM machines to make our lives easier and as they are true when dealing with large inputs. The reason is that when dealing with large inputs, the cache-disk time, the addition-multiplication time etc doesn't matter. This is also why we normally use RAM along with asymptotic analysis; as we don't care if we can do better in small cases; we want to do better in large ones.

For dealing with memory, concurency, parllelism, cache-disk etc other models of computation are created#footnote[RAM is a model in the class of models called Register Machines which are equivalent to Turing Machines.]. You will possibly study them in your wider career.

== What Big O doesn't want you to know?
$cal(O)$ sweeps the constents under the rug. This works when the rug is large and heavy while the constents are mere dust specks. 

But if, the constents are large: well then $10^(10^100) n$ is worse than $n^2$ for all values we could care about, irrespective of the fact the former is $cal(O)(n)$ while the latter is $cal(O)(n^2)$. This leads to something called Galectic Algorithms.

#definition(sub:"Galactic Algorithms")[A galactic algorithm is one with an optimal theoretical asymptotic performance, but which is never used in practice. Typical reasons are that the performance gains only appear for problems that are so large they never occur, or the algorithm's complexity outweighs a relatively small gain in performance. Galactic algorithms were so named by Richard Lipton and Ken Regan, because they will never be used on any data sets on Earth.]

This is of matter to us as at the end of all this theoretical work, we wish to use our algorithms or atleast know when to switch algorithms. For example, a lot a languages use insertion sort to sort till $n=5$ or $6$ before going to merge sort.

Hence, sometimes one really does the grulling analysis with the constents we saw in chapter 7.

This may make it seem useless. But the fact is, an algorithm, even if impractical, may create new techniques that may eventually be used to create practical algorithms.

Also, an impractical algorithm can still demonstrate that conjectured bounds can be achieved, or that proposed bounds are wrong, and hence advance the theory of algorithms.
#quote(sub : "Richard J. Lipton, Kenneth W. Regan")[
This alone could be important and often is a great reason for finding such algorithms. For example, if tomorrow there were a discovery that showed there is a factoring algorithm with a huge but provably polynomial time bound, that would change our beliefs about factoring. The algorithm might never be used, but would certainly shape the future research into factoring.]

= An Informal Survey of Multiplication Algorithms
The word Algorithm orignates from French where it was the mistranslated name of the 9th Century Arabic scholer Al-Khwarizmi, who was born in present day Uzbekistan, who studied and worked in Baghdad. His text on multiplying indo-arabic numerals travelled to Europe and his name was mis translated to "Algorisme" which later evolved into algorithm. While we will see other algorithms of the ancients in the excercise, let's end the main text with the OG multiplication. We will consider the multiplication of two $n$ digit numbers, given it takes a single operation to solve for $n=1$ (base cases). Our model of computation will be RAM but without the assumptions on addition and multiplication.

We assume additions of sigle digit takes $cal(O)(1)$ (constent) time, and hence, adding a $m, n$ digit number takes $cal(O)(min(m,n))$ time. This is realized by the school book carry method of additon and is optimal #footnote[The proof for the optimality is much harder. It was given by Emil Jerabek in 2023 using a technique we will see in ch 11 called Amortization]. Similer proof holds for subtraction.

```
-- | Addition and Subtraction of two numbers
-- Note, we are taking the numbers in reverse the usual order. That is Ones -> Tens -> Hundreds ...
addNum num1 num2 = addNumCarry num1 num2 0 where
  -- Both numbers exhausted : if carry is 0, end otherwise append the carry.
  addNumCarry [] [] k = if k == 0 then [] else [k]
  
  -- First number exhausted: add carry to remaining digits of second number
  addNumCarry [] (y:ys) k = 
    let (a,b) = (k+y) `divMod` 10 
    in if a == 0 then b:ys else a:b:ys
  
  -- Second number exhausted: add carry to remaining digits of first number
  addNumCarry (x:xs) [] k = 
    let (a,b) = (k+x) `divMod` 10 
    in if a == 0 then b:xs else a:b:xs
  
  -- Main case: add corresponding digits plus carry, propagate new carry
  addNumCarry (x:xs) (y:ys) k = 
    b : addNumCarry xs ys a 
    where (a,b) = (x+y+k) `divMod` 10  -- a is new carry, b is digit result


subNum :: [Int] -> [Int] -> [Int]
subNum num1 num2 = subNumBorrow num1 num2 0 where
  subNumBorrow [] [] b = if b == 0 then [] else error "Result would be negative"
  subNumBorrow [] (y:ys) b = error "Result would be negative"
  subNumBorrow (x:xs) [] b = 
    let diff = x - b 
    in if diff < 0 
       then 9 : subNumBorrow xs [] 1  -- borrow from next digit
       else if diff == 0 && xs == [] then []  -- remove leading zeros
       else diff : xs
  subNumBorrow (x:xs) (y:ys) b = 
    let diff = x - y - b
    in if diff < 0 
       then (diff + 10) : subNumBorrow xs ys 1  -- borrow from next digit
       else diff : subNumBorrow xs ys 0
```

The naive way to multiply would be to define multiplication as repeated addition. Something of the sort `multiply 1 b = b` and the reccurence. `multiply a b = b + multiply (a-1) b`. For two `n` digit numbers, this will take $cal(O)(10^n) dot cal(O)(n) = cal(O)(n 10^n)$ operations. We normally use this to define single digit multiplications (as writing the table by hand is too cumbersome and in one digit case, it works just fine).

```
-- | Singluar Digit Multiplication
mulDig :: Int -> Int -> [Int]
mulDig 0 _ = [0]
mulDig _ 0 = [0]
mulDig dig1 dig2 = [dig1] `addNum` mulDig dig1 (dig2-1)
```


An improvement in the multiplication algorithm, we are already familier with is the one taught in school. This was also the algorithm Al-Khwarizmi found. 

```
-- | School Book Multiplication

-- Multipling a number with a digit
mulNumDig :: [Int] -> Int -> [Int]
mulNumDig num dig = foldl1 (\a b -> a `addNum`  (0:b)) (map (mulDig dig) num)

schoolMul :: [Int] -> [Int] -> [Int]
schoolMul num1 num2 = foldl1 (\a b -> a `addNum`  (0:b)) (map (mulNumDig num2) num1)
```

The number of operations fort this algorithm is $cal(O)(n^2)$ as we multiply all the digits in the latter number with the digits in the former number and then add the results suitably one by one. That is in $32 * 45$, we will compute $32 * 5$ and $32 * 4$ and add them.

Doing a lot better than this took about a millenia, with the improvement comming from Anatoly Karatsuba in 1962. The idea used is the usual divide and conquor.

We divide the fist number into $x = a*10^(n/2) + b$ and the second number as $y = c*10^(n/2) + d$. Thus,
$
x y = (a*10^(n/2) + b) * (c*10^(n/2) + d)\
= a c * 10^n + b c * 10^(n/2) + a d * 10^(n/2) + b d
$

```
-- | Naive Divide and Conquor defination
divNcon :: [Int] -> [Int] -> [Int]
divNcon [x] num = mulNumDig num x
divNcon num [x] = mulNumDig num x
divNcon num1 num2 = let
  n = length num1
  n2 = n `div` 2
  (a,b) = splitAt n2 num1
  (c,d) = splitAt n2 num2
  ac = divNcon a c
  ad = divNcon a d
  bc = divNcon b c
  bd = divNcon b d
  n2zeros = replicate n2 0
  nzeros = replicate n 0
  in ac `addNum` (n2zeros ++ bc) `addNum` (n2zeros ++ ad) `addNum` (nzeros ++ bd)
```

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

Here is an Haskell implementation of the same
```
-- | Karatsuba Multiplication algortithm
karatsurba :: [Int] -> [Int] -> [Int]
karatsurba [x] num = mulNumDig num x
karatsurba num [x] = mulNumDig num x
karatsurba num1 num2 = let
  n = length num1
  n2 = n `div` 2
  (a,b) = splitAt n2 num1
  (c,d) = splitAt n2 num2
  ac = karatsurba a c
  bd = karatsurba b d
  abcd = karatsurba (a `addNum` b) (c `addNum` d)
  adPlusbc = (abcd `subNum` ac) `subNum` bd
  n2zeros = replicate n2 0
  nzeros = replicate n 0
  in ac `addNum` (n2zeros ++ adPlusbc) `addNum` (nzeros ++ bd)
```

This is a lot better. The next improvement came just an year later in 1963 by Tooom and Cook, making it $cal(O)(n^(log_3(5)))$. Here is what we belive their research process looked like:
#figure(
  image("../images/multiplication-algo-meme.png", height: 50%))

If you are wondering, they showed that we can break the multiplication in five $n/3$ sized products. Actually, we can split in any number of parts we want. Karatsuba is Toom-2, the $cal(O)(n^(log_3(5)))$ algorithm is Toom-3.  When split in some $k$ parts, the complexity is $cal(O)(n^E)$ where $E = log_k(2k − 1))$.

This can in theory do $cal(O)(1)$ multiplication. As we have seen in the section about the dark secrets of big-oh, the constents will cause the problem. Doing an exact complexity analysis can allow us to compute the exact speed of growth of the constent of $cal(O)(n^E)$ (hint: It is basically exponential).

#exercise(sub : "Toom-Cook")[
  Making the required modefications to the haskell implementation of karatsurba, implement Toom-3 algorithm.
]

This leads us to the $cal(O)(n log(n) log(log(n)))$ Schönhage–Strassen algorithm (1971) which uses the Discrete Fast Fouries Transform algorithm described in chapter 8. The exact implementation can be found in the appendix, if you are morbidly curious regarding the same. In this paper, Arnold Schönhage and Volker Strassen also conjectured a lower bound of $Omega(n log(n))$.

This is about the end of multiplication algorithms I can hope to talk about with the material in this book. Also, the upcomming algortithms are @definition_of_Galactic_Algorithms which is why most implementations (even in the most complex matehamtical computation software) use Karatsuba or Toom-3 till some size and then switch to Schönhage–Strassen. So everything here onwards are just fun facts.

The next leap came in 2007, when Martin Fürer improved the bound to $cal(O)(n log(n) 2^(cal(O)(log^*(n))))$ where the $log^*(n)$ denotes the number of times we must take $log(n)$ before we go below $1$. This leap was made possible due to half-DFT's, lots of ring theory and complex analysis and certain results about primes of form $p = 2^(2^k) + 1$ turining up true. This algorithm beats Schönhage–Strassen for integers with about $10^19$ digits.

The next improvement came by using number theory inplace of complex analysis (bundled with other suitable changes) courtasy Anindya De, Piyush P Kurur, Chandan Saha and Ramprasad Saptharishi#footnote[Who was at the same institute (Chennai Mathematical Institute) as the authors when he made this algortithm. The other authors were from IIT Kanpur.] in 2008. Their algorithm beats this Fürer for numbers with about $10^4796$ digits.

In 2015, David Harvey, Joris van der Hoeven and Grégoire Lecerf gave a new algorithm which replaced the $cal(O)(log^*(n))$ in Fürer with $3 log^*(n)$. The only issue is that this paper used certain unproven conjuctures on Mersenne primes.

In 2015-16, in a series of two papers, Svyatoslav Covanov and Emmanuel Thomé first made a new algorithm with same complexity as Fürer and then, using unproven conjuctures on Fermat Primes and genralizations, produced an algorithm where $cal(O)(log^*(n))$ is replaced with $2 log^*(n)$.

Not to be defeated so easily, Harvey and Hoevan snapped back in 2018 with an algorithm which acives the same complexity as Covanov and Thomé without any conjuctures. They insread used Minkowski's theorem (which funnily was proven in 1889).

And to seal the deal, Harvey and Hoeveen published the first $cal(O)(n log(n))$ multiplication algorithm in March 2019.

#quote(sub : "David Harvey and Joris van der Hoeven")[...our work is expected to be the end of the road for this problem, although we don't know yet how to prove this rigorously.”]

So well, can we do better is still an open question.

Anyways, for this paper, they shared the de Bruijn medal in 2022. With that, we have come full circle I guess.


// = Exercise
// #exercise(sub : "A strange pair?")[
//   Can you find a pair of functions such that $f(n) != o (g(n))$ and $f(n) != Omega(g(n))$?

//   What if they both had to be monotonic increasing?
// ]


// At some point, I'll fix the absolute values in some fractions. As we fix g ourselves, we neednot have absolute there in some defninitions. 