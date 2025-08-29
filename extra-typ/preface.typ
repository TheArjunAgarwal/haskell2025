#pagebreak()
#import "../Modules/Chapter.typ" : title_page
#title_page(title:[Preface])
#counter(heading).update(0)
#set heading(offset : 1)

= Why you should care about Haskell

== If You Like Programming

=== Influence on Programming Language Design

Haskell pioneered features that are now mainstream:

- Type inference - now in TypeScript, Kotlin, Scala

- Algebraic data types - seen in Rust, Swift

- Monads - show up in asynchronous/await and error handling patterns

- Pattern matching - used in modern JS, Python (3.10+), and Scala

Haskell is like the *testbed* where many modern language ideas are born.

The academic research community that develops and creates the features of modern programming languages highly prefer Haskell as their conceptual base and prototyping testbed.

=== Understandabilty, Readability and Refactorability

There is a common stereotype that once a large program is written and debugged, it usually gets so complicated that it is better to just change even a single letter of the code-base.

This leads to major problems when the functionality of the program needs to be expanded or changed. In the software engineering / corporate setting, this is known as the "refactorabilty" problem.

Haskell, by design, disallows and discourages this. Haskell code has many properties that make it very understandable and readable. If you have a baseline understanding of Haskell, it will not be easy to end up in a situation where you don't know what your own code is trying to do.

It also makes Haskell very readable to such an extent that it is often the case that just reading one line of a function definition is enough to understand what that function is supposed to do.

And obviously, this makes Haskell celebratedly one of the most refactorable languages.

=== Makes you a Better Programmer

The concepts in Haskell which make the above things true can be applied to any language, it's just that while Haskell by its very structure forces / guides you into it, in other languages it's up to your own discipline to do so.

That means that once you learn Haskell, you will be able to apply all these concepts with their extremely useful consequences in any programming language, and thus become a better programmer.

=== Catches Bugs before Running

Haskell has a thing called "types" which is purely logical, but eliminates nearly all possible bugs even before the first time that you run the program.

=== Parallelism

tbd

=== Builds Safety-Critical Software

In military systems, spaceflight, and financial systems, making sure that code works as intended and that there are no bugs is a strict and extremely important necessity.

In this use-case, Haskell's properties of having nearly no bugs, being correct by construction etc. are highly prized, as evidenced by its use by -

- NASA (the American spaceflight organization)
- Galois (security software for NSA and NASA)
- Anduril (the famous military infrastructure startup)
- Cardano (a currency based on blockchain infrastructure, fully written in Haskell)
- IOHK (one of the biggest blockchain providers)
- Bitnomial (cryptocurrency options and futures firm)
- Standard Chartered (global banking firm)

=== Used by Prestigious People and Organizations

Facebook uses Haskell for one of the best spam detection systems in the world.

Y Combinator is one of the largest startup advisory and venture capital funding organizations, which helped launch the likes of AirBnB, Twitch, Reddit, DropBox, GitLab and Zepto.
\ Its founder, Paul Graham, is a huge proponent of the functional programming paradigm (which Haskell follows) , and credits it as one of the reasons for the success of his own startup.

Jane Street is one of the most famous quantitative trading firms in the world. It uses OCaml (another functional-paradigm programming language) as the primary language and backbone of its code-base.

Haskell is very useful in analyzing, comparing and transforming textual data. That is why Pandoc, the premier software for c0nverting between file formats (word to pdf, latex to typst etc.) is written in Haskell. Semantic, the software that GitHub uses to compare different programming languages and process data for the Copilot AI is also uses Haskell.

And the list goes on...

== If You Like Mathematics

=== Haskell is Math

Haskell is pretty much as close to math as you can get in terms of programming languages. The language is wholly based on the extremely mathematical ideas of induction, recursion, domain, co-domain, \~free-generation\~, \~currying\~, and \~category theory\~.

Even if you don't know all of these concepts yet, what you can assume is that if your basic mathematical concepts are clear, then writing in Haskell should, in principle, not be extremely different from writing precise mathematics.

And hopefully that gets you a bit interested.

=== Math begets Haskell

Haskell is wholly founded on mathematical concepts. Nearly all of the ideas it uses first appeared in the minds of various mathematicians and logicians. 

So Haskell is built upon a long history and rich tradition and mathematical thought. 

So you can choose to treat Haskell as a way to see how mathematical concepts can work beautifully in action and explore the real-life uses abstract math can have.

=== Haskell begets Math

Haskell can actually help you learn new math and solidify you r current knowledge of math.

In this course, you will learn about - 
- Precision, i.e., a deeper understanding of structure of mathematical writing
- Currying, which is extensively used throughout all of math, and you should see it in Linear Algebra.
- Types, which are a very useful mental tool for understanding and checking proofs, and can  be applied to digest proofs faster and better throughout your life
- Category Theory (a little bit), which is the theory of maps from various mathematical objects to other ones

=== Proof Assistants

Proof Assistants are tools that let mathematicians verify a proof beyond any doubt. They are like programming languages that you can write a formal proof in, and the computer will verify whether the proof works or not.

This has become an interesting topic in the discussion about the future of mathematics, and many famous mathematicians (such as Terrence Tao) believe that proof assistants will be an indispensable tool for the progress of mathematics.

As many proof assistants, such as Agda, Coq, and Lean can be thought of as extension based on or built up from the concepts used in Haskell, knowing Haskell should provide advantage in understanding how to write proofs using such assistants.

=== Used in Mathematical Computation

Haskell is used in computing various mathematical objects and to carry out mathematical experiments which test some statement by  generating examples or counterexamples.

This generated data can be used to disprove mathematical conjectures or make new ones.

=== Expressivity of Haskell

You might surprised by the depth and breadth of mathematical objects that can be defined in Haskell.

For example, one can define a list of ALL the prime numbers.

= How to Learn Haskell

== If You Like Math

If you follow this book, you will see that Haskell follows a mathematical style as closely as it can. So, if you can learn to express a few mathematical ideas precisely enough, Haskell _should_ become child's play. 

In order to test your knowledge and be sure that you are learning the correct meanings of things, it is encouraged that you try coding all the assignments, graded or otherwise. Also, it should be helpful to practice programming by getting problems from other sources as well. 

The beauty of practicing programming is that anyone can check their own solution by _running_ a few examples, so you can get lots of practice and _verify it by yourself_ too.

But most importantly, remember to have fun!

== If You Like Programming

If you have dabbled in the functional paradigm before, you know what you are getting into.

But if the languages you have dealt with in the past have been mainly procedural, object-oriented, etc., then you need to read these next sections VERY CAREFULLY.

Haskell is NOT a programming language in any sense of "programming language" that you have encountered before. Often when some concept comes up that looks or feels similar to programming you might have done in the past, please be very careful to not jump to assumptions. The functional paradigm rarely aligns with any of the concepts of other paradigms.

Possibly the best way to avoid such assumptions would be to -

#align(center)[*Just think of Haskell as a way of writing mathematical expressions, \ which only happens to be runnable by mere coincidence.*]

Basically, treat Haskell code as more of math notation than programming syntax, and you should be fine.

Keep your mind open, and you might just learn something that changes your entire perspective on programming for the better.

But most importantly, have fun!

// - Ryan Hota. This is his third
//     year doing it, and at this point, 
//     he could teach the course in his sleep. And sometimes does, due to 
//     poor scheduling His research interest is Logic and Set Theory, which 
//     is a fancy way of saying he enjoys pain. 
//     He uses Windows out of what we can only assume is a deep and personal 
//     vendetta against usability.

// - Shubh Sharma should, at this 
//     very moment, be writing his thesis. 
//     Instead, he's co-authoring a textbook. His academic timeline is 
//     nonlinear and possibly violates causality. He runs Linux, because of course 
//     he does. Ask him a question and he’ll answer it in a shell script. 
//     Nobody knows his actual research interests, including he himself.

// - Arjun Maneesh Agarwal, the    most overconfident student to walk CMI’s 
//     hallowed halls. He’s under the delusion that he can overhaul a
//     course with nothing but ambition, caffeine, and opinions. 
//     His research interests are Game Theory, Economics, and Statistics, 
//     which explains why he thought he could outvote two people and make us use 
//     LaTeX. We use Typst now. Take that, Arjun. Also, he uses MacOS because of course he does.

// This book is our open letter to the future 
// students of CU1101, and our way of making sure that, when the class is inevitably 
// postponed again for an NBA final, you’ll at least have something to read.

// Eventually, Prof. Suresh will probably write a real preface here. 
// Something dignified. Academic. Wise. But until then, 
// consider this the Haskell version of a Reddit shitpost—well-typed, 
// lovingly chaotic, and just expressive enough to make you wonder if we’ve all gone mad. (We have.)

// Enjoy.

// -- Chat GPT
= How to Read this Book

#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise

#def()[Definitions look like this.]

#exercise[Exercises look like this.]

```
Code looks like this.
```

You can use the link "Contents" in the upper-left corner of every page to jump back to the Table of Contents.

Many things, especially #text(fill:purple)[coloured text], are links. You can exploit that as you wish.

Happy reading!