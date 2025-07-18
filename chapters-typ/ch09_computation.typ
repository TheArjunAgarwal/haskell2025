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
#definition(sub : "Big A notation")[
  The symbol $cal(A)$ means absolutly atmost. For example, $cal(A)(4)$ refers to a value whoose absolute value is less than or equal to $4$. For example, $pi = cal(A)(4)$. 
]

Note, the $=$ sign is not trasitive in this regard. The reason for this is because the notation came from math and unlike Computer scientists, mathematicains have a small vocabulary and couldn't think of another symbol. As a Mathematician once put in words(quite surprising),

#quote(sub : "De Bruijn")[Mathematicians customarily use the $=$ sign as they use the word “is” in English: Aristotle is a man, but a man isn’t necessarily Aristotle.]

Comming back to mathematics, Big-A notation is very compatible with arithmatic.
#proof(thm : [
  + $pi = 3.14 + cal(A)(0.005)$
  + $10^(cal(A)(2)) = cal(A)(100)$
])[
  We will just use the defination of $-k <= A(k) <= k$.

  Thus, $pi = 3.14 + cal(A)(0.005) <==> pi in [3.14 - 0.05, 3.14 + 0.05]$ which is true as $3.140 < pi = 3.1415926 dots < 3.142$.

  Similerly, $10^(cal(A)(2)) = cal(A)(100) <==> [10^(-2), 10^(2)] subset.eq [-100, 100]$ which is true.
]

#proof(thm: [$cal(A)(x) dot cal(A)(y) = cal(A)(x y)$])[
  We can eliminate the $cal(A)$ to get that the above statement is equivalent to, $[-x y, x y] subset.eq [-x y, x y]$ which is trivially true.
]

We can use the notation for variables as well,
#proof(thm:[
+ $sin x=cal(A) (1);$
+ $cal(A)(x) =x dot cal(A)(1)$;
+ $cal(A)(x)+cal(A)(y) =cal(A)(x+y) $, where$ x, y >= 0$;
+ $(1+cal(A)(t))^2 =1+3cal(A)(t)$, where $t=A(1)$.
])[
Left as an excercise to reader.
]

#exercise(sub : "Big A Analysis")[
  + Simplify: $(5 + cal(A)(0.2)) + (3 + cal(A)(0.1))$
  + Simplify: $(5 + cal(A)(0.2)) * (3 + cal(A)(0.1))$ 
  + Express $e^x$ for $|x| <= 1$ in the form $1 + cal(A)(u)$ for some $u = k x$. 
    *Hint*: Use the fact that $e^x = 1 + x + x^2/2! + dots$.
  + Define a sequence $x_0 = $, $x_n = 2 x_(n-1) + A(epsilon_n)$ where $epsilon_n > -$. Then solve for $x_n$ when
    + $epsilon_n = 1/(3^n)$
    + $epsilon = 1/n^2$
]

== The Hierarchy of Functions
#definition(sub : "Asymptotic Dominance")[
 Asymptotic Dominance is a relation over functions, where $f(n) prec g(n)$ when $g(n)$ approaches infinity faster than $f(n)$. We can formalize this by saying:
$
f(n) prec g(n) <==> lim_(n -> oo) (f(n))/(g(n)) = 0
$
]
#proof(thm : [Asymptotic diminence is transistive.])[
  
]
