#import "style.typ":*
#show: codly-init.with()
#codly(languages: codly-languages)

= Example of what a chapter looks like!

== Motivation for Functional Programming
Let’s start with a simple example to demonstrate the key differences between functional programming and imperative programming. 

The solution in an imperative language, say python looks like

```python
def sieve_of_eratosthenes(n):
    primes = []
    is_prime = [True] * (n + 1)
    is_prime[0] = is_prime[1] = False

    for num in range(2, n + 1):
        if is_prime[num]:
            primes.append(num)
            for multiple in range(num * num, n + 1, num):
                is_prime[multiple] = False

    return primes
```
On the other hand, a functional language, say Haskell solves it much more easily.

#haskell[```
sieveOfEratosthenes :: Integer -> [Integer]
sieveOfEratosthenes n = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
```]

While we need not understand the full details of the solution right now, notice that as Haskell is a functional language, we don't need to deal in states of the programme.

This means that a lot of errors which imperative languages run into, due to overwriting or trying to access non-existent data or deleted data etc can be avoided in a stateless system, or a functional system.

So why are functional languages not popular?

Let's take a look at the history of functional programming:
1950s — Functional programming was invented, but not popular because it took more memory to be stateless. Memory was expensive.

1980s — Object Oriented Programming (OO) became popular in the 80s because GUIs became popular. In OO style, it’s easier to program things that use a fixed number of operations for an unlimited number of operation. This was supported by the imperetive languages.

2010s — Memory is cheap. But we can’t make transistors any smaller (marginal gains in hardware capability). Fast processing and big data processing requires more than one core. There has been an increasing emphasis on asynchronous, distributed, multi-core computing. Multicore competing for the same memory bus, OS no longer manages threads for you on multicore — if you need to perform the same tasks faster and faster, you can increase to unlimited number of cores. Stateful programming is more of a liability now with these new requirement. Functional -> no assignments -> no states -> no blocking or concurrency issues.#footnote[timeline copied from Xiaoyun Yang's blog]

Thus, it becomes essential that a mordern proggrammer be profficient in atleast one functional language. For us that will be Haskell as it is the most advanced and wide spread of the bunch(also as it will come in our exams).

== A short history of Haskell
Haskell is a purely functional programming language named after the logician Haskell Curry. Its development began in the late 1980s as a response to the growing need for a standardized functional programming language. The following key milestones mark Haskell's evolution:

Early Development (1987-1990): The Haskell language was born out of a workshop held in 1987, where researchers sought to create a common language to consolidate various functional programming languages like Miranda, Lisp, and Scheme. The first version of Haskell (Haskell 1.0) was released in 1990, introducing a strong static type system, lazy evaluation, and a rich set of features.

Haskell 98: In 1998, Haskell 98 was released as a stable version of the language, providing a standard that encouraged wider adoption and interoperability between Haskell implementations. It focused on simplicity, ensuring a core language that supported essential functional programming concepts.

Language Extensions and GHC: The Glasgow Haskell Compiler (GHC) became the most widely used Haskell implementation, incorporating numerous extensions to the standard language. These extensions enhanced Haskell's expressiveness, allowing for advanced features like type classes, template Haskell, and monadic programming.

Community and Libraries: Over the years, the Haskell community has grown, contributing a vast ecosystem of libraries and tools through platforms like Hackage and Stackage. The development of frameworks such as Yesod and Snap for web programming and libraries like QuickCheck for testing has further solidified Haskell's place in the programming landscape.

Modern Use Cases: Today, Haskell is used in a variety of domains, including finance, data analysis, and web development. Its unique features make it well-suited for applications requiring high reliability and maintainability, often seen in industries where correctness is paramount.

== Lambda Calculus and the Foundations of Haskell
Lambda Calculus was a branch of mathematics invented by Alonzo Church, a mathematician from Princeton. It basically tries to computationally work with functions by treating them as black boxes.

An interesting piece of history is that Alonzo was the PhD supervisor of Alan Turing who is famous for Turing Machines which captures a state based model of computing. There work together indicates that state based and functional model of computing are equivalent. This is called #emph{Church-Turing Hypothesis}.

Coming back to Lambda calculus, For example, let's consider the increment function: $x \to square \to  x+1$. This is a function where the $square$ is a black box which means we can't really look inside of it and it has no internal states. 

This is clearly quite different than Turing's models as they had internal states. However, this function, similar to mathematical functions, is just a machine which takes input and spits out output.

A rather convenient notation for these functions is $lambda x. x+1$ and $lambda x. lambda y. x*y$ for increment and product functions respectively.

The notation in general is 

$lambda i_1. lambda i_2 dots lambda i_n dot f(i_1, i_2, dots  , i_n)$

where $i_1, i_2, dots i_n$ are the required inputs while $f(i_1,i_2, dots i_n)$ is the output.

The use of $lambda("Lambda")$ before the inputs is the reason for the namesake of the function.

We also use the notation $(lambda x. x+1) 5$ to denote running the function for $5$ which in this case outputs a $6$.  In general, we just write the function is a bracket and the given inputs as a comma separated list next to it. 

At this point one may wonder what is the point of all this? To begin with, we can encode any program as Lambda calculus. While it is not pretty or practical for many programs, this does act as a tool for mathematical verification.

The second is that it forms the basis of functional programming(including Haskell). Functional programming languages at their core simply use glorified Lambda calculus.

And finally, it is now a part of most languages. Java, C, C\# and the list goes on. This makes it a must learn for all computer scientists. 

Another strange aspect of Lambda Calculus is that it has no inbuilt if and else, no recursion and no logical operators. Anything we need, we need to write it down as functions.

#example[Taking the definitions $"TRUE" = lambda x. lambda y. x$ and $"FALSE" = lambda x. lambda y. y$, define NOT, AND and OR functions,\\]

#solution[Notice that the $"TRUE"$ function and $"FALSE"$ function literally take two inputs and return the first and the second input unchanged respectively. We can use this property to simply define the not function as the output of $("TRUE")["FALSE", "TRUE"]$ and $("FALSE")["FALSE", "TRUE"]$.
Convince yourself that this actually works. To use it in general we just write it as $lambda b. (b)["FALSE", "TRUE"]$ where $b$ can be $"TRUE"$ or $"FALSE"$ and behaves like a function as denoted by the brackets.

We can use this same scheme to denote the AND function as $lambda a. lambda b. (a)(b)[("TRUE", "FALSE"),("FALSE", "FALSE")]$ and the OR function as $lambda a. lambda b. (a)(b)[["TRUE", "TRUE"], ["TRUE", "FALSE"]]$ where we first choose one of the pairs and then one element from the chosen pair. Convince yourself that this works. #footnote[Grahm Hutton on Lambda Calculus, Computerphile]]
== Some Terms
Computer-Science people like to use fancy terms. Here are some of them commonly used with respect to lambda Calculus.

#definition[Beta reduction is like simplifying a math expression by substituting values. In lambda calculus, you have functions (like formulas) that take inputs (like numbers). When you apply a function to an argument, you can replace the function's variable with that argument.]

#example[You have a function $lambda x .x + 1$ (which means "add $1$ to $x$").
If you apply it to $3$, you get $lambda x.x + 1 3$, which reduces to $3 + 1$, simplifying to $4$.]

#definition[A normal form is a way of saying that an expression is simplified completely, and you can't reduce it any further. Think of it like finishing a puzzle; you can't put any more pieces together.

In lambda calculus, an expression is in normal form if it doesn’t contain any function applications that can be simplified (beta-reduced).]
#example[
    $lambda x.x + 1$ is in normal form because it can't be simplified further.
    
    $lambda x.x + 1 3$ is not in normal form because you can still reduce it by applying the function to 3.]

#definition[
    Church encodings are a way to represent data and control structures (like numbers and boolean values) using only functions. It shows how powerful functions can be!
    
    We just did that for AND, OR and XOR functions in the example at the end of previous section. By the Church-Turing hypothesis, we can do it for a lot of things.
]
#example[
    Eta expansion refers to adding abstraction to another function.
    
    Consider the built in function \code{abs} which gives the absolute value of its argument. It can be eta expanded to $lambda x. abs x$.
  ]
Most of these are just names of things we already knew. But then again, how does one sound learned without such vocabulary.
== Using Lambda functions in Haskell
In Hasekll, one can use a lambda function as follows:
```haskell
    (\x -> x+1) 3
```
which will trivially give us $4$. Notice we replace the $lambda$ with $slash$ and $.$ with $->$.

We can write a multivariable function as:
```haskell
(\x -> \y -> x+y) 3 5
```
which will give us $8$.

As Haskell is smarter than simple lambda calculus,
``` haskell 
    (\x y -> x+y) 3 5
```
also gives the same result. Space seperated variables after a slash are taken as inputs by Haskell.

These lambda operators are used in Haskell to create temporary nameless functions. We will talk about them in more detail later.
== Haskell Fundamentals

#problem[A child is playing with a ball on the nth floor of a tall building. The height of this floor above ground level, $h$, is known. He drops the ball out of the window. The ball bounces (for example), to two-thirds of its height (a bounce of $0.66$). His mother looks out of a window $1.5$ meters from the ground. How many times will the mother see the ball pass in front of her window (including when it's falling and bouncing)?
Three conditions must be met for a valid experiment:
- Float parameter "h" in meters must be greater than 0\\
- Float parameter "bounce" must be greater than 0 and less than 1\\
- Float parameter "window" must be less than h.
If all three conditions above are fulfilled, return a positive integer, otherwise return $-1$.
Note: The ball can only be seen if the height of the rebounding ball is strictly greater than the window parameter.
]
#solution[
    [Solution]
    We need to think simple here. First we check if the conditions are initially met.
    ```haskell
    see :: Float -> Float -> Float -> Int
        see h bounce window
            |h <= 0                         = (-1)
            |bounce <= 0 || bounce >= 1     = (-1)
            |window >= h                    = (-1)
            |otherwise                      = ans h bounce window
        where 
            ans h bounce window = undefined
    ```
    Now let's think of the logic if the inital conditions are met.
    We can simply simulate the ball falling.
    
    For some $h$, if $h*"bounce" > "window"$ then we see the ball twice(once going down and once going up) and the new height is now $h*"bounce"$.
    Otherwise, we see the ball one last time. Let's code that now.
    ```haskell
        see :: Float -> Float -> Float -> Int
        see h bounce window
            |h <= 0                         = (-1)
            |bounce <= 0 || bounce >= 1     = (-1)
            |window >= h                    = (-1)
            |otherwise                      = ans h bounce window
        where 
            ans h bounce window 
                |h <= window        =0
                |h*bounce > window  = 2 + ans (h*bounce) bounce window
                |otherwise          = 1
    ```
  ]
#remark[
        A point to add here is that the `ans` function can written in a much faster and smarter fashion. We are basically looking for $n$ such that $h*"bounce"^n \leq "window"$.
        This can be simply done by taking log base of $"bounce"$.
        While I'll leave the full details of this implementation as excercise, Haskell has a pre-defined log function defined.
        We also wish to point out `^`, `^^` and `**` have minor but neccesory differences. The former raises integers to non-negetive powers, the middle raises fractions to any integral power. The latter raises a float to the power of another float.
      ]

