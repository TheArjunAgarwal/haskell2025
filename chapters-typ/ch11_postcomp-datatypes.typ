#import "../Modules/Definition.typ" : def
#import "../Modules/Exercise.typ" : exercise
#import "../Modules/Quote.typ" : quote
#import "../Modules/Proof.typ" : proof
#import "../Modules/Code.typ" : unligate
#import "@preview/mannot:0.3.0" : *

#let definition = def
#let example = it => [For example - \ #it]


= Datatypes (One last time) <ads>
We have discussed how we can use datatypes to reprasent data in a cleaner way.

Towards the end of that chapter, we had implemented a binary tree and mentioned that some algorihtms are implemented using binary trees. That seems to indicate that we might be interested in storing data using a data type for ease of working and then recovering the data? In this case, we call the datatype a data structure.

We will talk about some common data structures in this chapter.


= Stacks and Queues
Before moving to more complex data structures, we will talk about simpler data structures, which are degradingly called containers.
#definition(sub : "Containers")[
A data structure that permits storage and retrieval of data items independent of content is called a container. 
]

That is, the data structure has no interest in examining what we are storing and optimizing for that, it is purely a vessel for the data. This makes containers only interesting and distinguished by the insertion and retrieval order they support.

== Stack
#definition(sub : "Stack")[
    A container that supports insertion and retrieval in a first in last out fashion is called a stack.
    
    It is as if we have a stack of books, we can add things to the top and remove from the top. 

    The insertion and retrieval in this case are called push and pop.
]
The term push and pop originate from Samelson and Bauer's paper on evaluating expressions (we will see it in a moment), where they called this container cellar. 

A cellar is an underground storage vertical hole used to store wine casks. So to insert a new cask, we will need to push it in and to retrive a new cask, we will need to pull it upward (which normally made an audible pop). We sometimes provide two more functions `peek` and `empty` which allow us to inspect the top element as well as check if the stack is empty.

We can implement this in Haskell as
```
module SafesStack where

data Stack a = Empty | Top a (Stack a) deriving Show

empty :: Stack a -> Bool
empty Empty = True
empty _ = False

pop :: Stack a -> Maybe (a, Stack a)
pop Empty = Nothing
pop (Top a s) = Just (a, s)

push :: a -> Stack a -> Stack a
push a s = Top a s

peek :: Stack a -> Maybe a
peek Empty = Nothing
peek (Top a _) = Just a
```

and an unsafe version for easier use:

```
module Stack where

data Stack a = Empty | Top a (Stack a) deriving Show

empty :: Stack a -> Bool
empty Empty = True
empty _ = False

pop :: Stack a -> (a, Stack a)
pop Empty = error "Empty Stack"
pop (Top a s) = (a, s)

push :: a -> Stack a -> Stack a
push a s = Top a s

peek :: Stack a -> Maybe a
peek Empty =  error "Empty Stack"
peek (Top a _) =  a
```

The official haskell version is unsafe and we will use the unsafe version throughout the section.

#exercise(sub: "Stack from List")[
    It is not hard to notice that the list we are familiar with are very similar to stacks. Can we implement stack using list?

    ```
    data Stack a = Stack [a]

    empty :: Stack a -> Bool
    pop :: Stack a -> Maybe (a, Stack a)
    push :: a -> Stack a -> Stack a
    peek :: Stack a -> Maybe a
    ```
]

Remember, the `data` keyword obscures the underlying structure when called as a `module`. This would make it impossible for someone to access the list, unless they are in the file defining the container itself. Anyways, so why is this rather simple structure any useful?
#exercise(sub : "Reverse Polish Notation")[
    One of the first uses of computers was to compute arithmetic equations. The brackets were however a kludge to deal with.
    $
    3 * 4 + (6 slash 3 - 5) - 8 slash 2 
    $ 
    is quite non trivial for computers to deal with. So what do we do about it?

    One idea, proposed by Charles Hamblin was using a reversed version of Jan Łukasiewicz's bracket less notation. Łukasiewicz proposed writng arithmetic in operator first, inputs second way that is: $2 + 3$ is transformed into $+ quad  2 quad 3$. Similerly, the above expression is transformed into:
    $
    - quad  + quad  * quad  3 quad  4 quad  - quad  slash quad  6 quad  3 quad 5 quad  slash quad  8 quad  2
    $
    The computation is easier to show by example than describe in words
    $
        &quad - quad  + quad  markrect(* quad  3 quad  4) quad  - quad  slash quad  6 quad  3 quad 5 quad  slash quad  8 quad  2 \
        &= quad - quad  + quad  12 quad  - quad  markrect(slash quad  6 quad  3) quad 5 quad  slash quad  8 quad  2 \
        &= quad - quad  + quad  12 quad  markrect(- quad  2 quad 5) quad  slash quad  8 quad  2 \
        &= quad - quad  markrect(+ quad  12 quad  -3) quad  slash quad  8 quad  2 \
        &= quad - quad 9 quad markrect(slash quad  8 quad  2) \
        &= quad markrect(- quad 9 quad 4) \
        &= 5 \
    $
    The idea of reverse polish notation is to keep the inputs first and the operator second: $2 + 3$ is transformed to $2 quad 3 quad +$. This transforms our expression to:
    $
    3 quad  4 quad * quad 6 quad 3 quad slash quad 5 quad - quad +  quad 8 quad 2 quad slash quad -
    $
    Can you make a scheme to evaluate this? Can you make a computer evaluate this?
]
The idea we expect you to see is that we keep adding the numbers and operations to the stack. As soon as we see an operator, we perform the operation on the first two elements of the stack and push that to the stack. We continue doing so till we have read the input completely.

$
& quad markrect(3 quad  4 quad *) quad 6 quad 3 quad slash quad 5 quad - quad +  quad 8 quad 2 quad slash quad - \
=& quad  12 quad 6 quad 3 quad slash quad 5 quad - quad +  quad 8 quad 2 quad slash quad -\
=& quad  12 quad markrect(6 quad 3 quad slash) quad 5 quad - quad +  quad 8 quad 2 quad slash quad -\
=& quad  12 quad markrect(2 quad 5 quad -) quad +  quad 8 quad 2 quad slash quad -\
=& quad  markrect(12 quad -3 quad +)  quad 8 quad 2 quad slash quad -\
=& quad  9  quad markrect(8 quad 2 quad slash) quad -\
=& quad  markrect(9  quad 4 quad -)\
=& quad 5\
$

The implementation would look like:
```
data Operator = Plus | Minus | Mul | Div deriving Show

evaluate :: [Either Integer Operator] -> Integer
evaluate expression = go expression Empty
  where
    go [] acc = peek acc
    go (token:xs) acc = case token of
        Left num  -> go xs (push num acc)
        Right op  ->
            let (rhs, acc1) = pop acc
                (lhs, acc2) = pop acc1
            in case op of
                Plus  -> go xs (push (lhs + rhs) acc2)
                Minus -> go xs (push (lhs - rhs) acc2)
                Mul   -> go xs (push (lhs * rhs) acc2)
                Div   -> go xs (push (lhs `div` rhs) acc2)
```
The time complexity is clearly $O(n)$. On my computer, I was able to evaluate a 300 thousand term expression in half a second.

#exercise(sub : "Ease of Use")[
    It is rather cumbersome to type `[Left 3,Left 4,Right Plus,Left 5,Left 6,Right Plus,Right Mul]` in place of `"3 4 + 5 6 + *"`. Make a function `convert :: String -> [Either Integer Operator]` to add some ease of use.
]
We will now give you as exercise to write a infix calculator (which can calculate arithmetic in the usual notation) using the below described algorihtm.
#exercise(sub : "Samelson & Bauer's Sequential Formula Translation")[
    Translate the following algorithm to Haskell. You will need to use two stacks, one of type `Stack Integer` and another of type `Stack Operator`. Also, parenthesis are also operators now. 

    - Read the expression from left to right. Let the token in consideration to be `x`
        - If `x` is an integer, push `x` to the Integer stack.
        - If `x` is an operator, 
            - Evaluate operators from operator stack till either
                - Operator stack is empty
                - the top of the operator stack is an open parenthesis
                - The precedence of the operator at the top of operator stack is lower than `x`
            - Push `x` onto the operator stack
        - If `x` is an open parenthesis, push `x` onto the operator stack.
        - If `x` is a closed parenthesis,
            - Evaluate operators untill an open parenthesis is at the top of the operators stack.
            - Remove the open parenthesis.
    - If the input string is empty, evaluate the remaining operators.

The precedence of operators is `/` = `*` > `+` = `-`. Note, equal precedence is not lower/higher precedence.

Evaluating an operator means popping two elements from the number stack, say $a$ and $b$ respectively and popping one element from the operator stack, say $plus.circle$ and returning $b plus.circle a$ ($a$ was popped first). 
]

A question you might have is if stack is just a more constrained list, why not just use the list? Well, for haskell, at compile time, there is no real difference. Stack is a historically significant data structure and is sometimes makes the algorithm more clear (as with the above examples). We think of a list and stack, just by the words in a different manner and hence, it is just a thinking tool.

It is also rather similer to a lot of humane thinking we do.
#exercise(sub : "Mass of a Molecule")[
An organic molecule can be defined as a sequence of atoms and represented by a chemical formula consisting of letters denoting these atoms. E.g. letter H denotes atom of hydrogen, C denotes atom of carbon, O denotes atom of oxygen, formula CH3COOH represents the molecule acetic acid consisting of two atoms of carbon, four atoms of hydrogen and two atoms of oxygen.

To write formulas efficiently, letters can be grouped in parentheses, and groups may be nested. Consecutive identical atoms or groups can be compressed with a number, e.g. COOHHH = CO2H3, and CH(CO2H)(CO2H)(CO2H) = CH(CO2H)3. A number after a letter or group is always between 2 and 9. A mass of a molecule is the sum of masses of all atoms: H = 1, C = 12, O = 16.

Write function `mass :: String -> Integer` to find the mass of a given organic molecule.

Examples
```
mass "CH3(COOH)" = 60 -- Acetic Acid aka Vinegar
mass "CH2(COOH)2" = 104 -- Malonic Acic, found in citrus fruits
mass "CH3C(CH3)CHCH2CH2C(CH3)CHCH2(OH)" = 154 -- Geraniol, the rose smell
```

Hint : You want to use a `Stack (Either Integer Char)` and the only `Char` we are intrested in are the parentheses.
]

== Queue
#definition(sub : "Queue")[
    A container that supports insertion and retrieval in a first in first out fashion is called a queue.
    
    It is similar to a line at a ticket counter, you can only join at the back and exit (with a ticket) at the front.

    The insertion and retrieval in this case are called enqueue and dequeue.
]
Unlike stack, it is somewhat harder to think of an effcient way to implement queues. A naive solution will be:
```
data NaiveQueue a = Queue [a] deriving Show

empty :: NaiveQueue a -> Bool
empty (Queue lis) = null lis


enqueue :: a -> NaiveQueue a -> NaiveQueue a
enqueue a (Queue lis) = Queue (lis ++ [a])

dequeue :: NaiveQueue a -> (a, NaiveQueue a)
dequeue (Queue []) = error "empty"
dequeue (Queue (x:xs)) = (x, Queue xs)


peek :: NaiveQueue a -> a
peek (Queue []) = error "empty"
peek (Queue (x:xs)) = x
```

The issue is that our enqueue function takes $O(n)$ time where $n$ is the number of elements already in the queue. This is a glaring inefficiency! Can we do any better?
```
data BatchQueue a = Queue [a] [a] deriving Show

empty :: BatchQueue a -> Bool
empty (Queue f r) = null f && null r

enqueue :: a -> BatchQueue a -> BatchQueue a
enqueue a (Queue f r) = Queue f (a:r)

dequeue :: BatchQueue a -> (a, BatchQueue a)
dequeue (Queue [] r) = dequeue (Queue (reverse r) [])
dequeue (Queue (f:fs) r) = (f, Queue fs r)

peek :: BatchQueue a -> a
peek (Queue [] r) = peek (Queue (reverse r) [])
peek (Queue (f:fs) _) = f
```

We are storing two lists with one being the front of the queue and the other being the back. But one might, rightfully ask, what does this added complexity get us? We are using a `reverse` making the dequeue $O(n)$ in the worst case. That makes this no better, right?

Not really. In the last chapter, we had mentioned amortization. It's time to talk about it.

=== Amortization

#definition(sub : "Ammortization")[
    The action or process of gradually writing off the initial cost of an asset is called amortization.
]
Amortization is an accounting term which is genrally used by firms to slowly write off expensive purchases. The concept is:

Say Prof. Wupendra Wulkarni buys a new (and expensive) laptop worth $20000 M\$$ to play chess on. On any 'work' related purchases, The University of Baker Street (TUBS) offers a payback upto $400 M\$$. 

Instead of being honest and reporting the purchase at once, Wupendra can instead report a cost for renting a computer every month (from himself) which conveniently comes to be $400 M\$$. This scheme will allow him to slowly write off the purchase over $50$ months. Note, Wupendra paid the price for the laptop at once, he is just doing the write off over a period of time.

This is called Ammortization in accounting. It is genrally a valid practice and is often done to prevent showing a quatar with some purchases as a loss making quatar.

We have a similar concept in the study of data structures as well. Robert Tarjan in 1985 formalized the concept of Ammortized analysis in CS as:

#definition(sub : "Ammortized Analysis")[
    Given a sequence of operations, we may wish to know the running time of the entire sequence, but not care about the running time of any individual operation. For instance, given a sequence of $n$ operations, we may wish to bound the total running time of the sequence by $O(n)$ without insisting that each individual operation run in $O(1)$ time. We might be satisfied if a few operations run in $O(log n)$ or even $O(n)$ time, provided the total cost of the sequence is only $O(n)$.

    Formally, To prove an amortized bound, one defines the amortized cost of each operation and then proves that, for any sequence of operations, the total amortized cost of the operations is an upper bound on the total actual cost. 
    $
    sum_(i = 1)^n a_i >= sum_(i=1)^n t_i
    $
    where $t_i$ is the actual cost of operation $i$ and $a_i$ is the ammortized.
]
To illustrate, we will prove that enqueue and dequeue are $O(1)$ ammortized. We will prove this by induction.

Note, the actual costs of enqueue and dequeue are $O(1) = c_1$ and $O(k) = c_2 k$ where $k$ is number of enqueue's before the last dequeue respectively. We will reprasent the chain of operations using $e$'s and $d$'s meaning enqueue and dequeue respectively.
#proof(thm: [
    enqueue and dequeue are performed in $c_0 = c_1 + c_2 = O(1)$ time ammortized.
])[
    B: For $n=1$, the only operation we can make is $e$ which is consistent with the claim as $c_1 <= c_0$.

    For $n=2$, we can either do $e,d$ or $e, e$. Note, this is also consitstne twith the claim as $c_1 + c_2 <=2 c_0$ and $c_1 + c_1 <= 2 c_0$.

    S: Assume that for any chain of operations of length $k < n$, it is bounded by $c_0 k$. All chains will be of the form
    $
    underbrace(e dots d, k) quad underbrace(e dots e, n-k-1) quad d
    $
    This will take time
    $
    t &<= c_0 k + c_1 (n-k-1) + c_2 (n-k-1)\
    &<= c_0k + (c_1 + c_2) (n-k-1)\
    &<= c_0 k + (c_0) (n-k-1)\
    &<= c_0 (n-1)\
    &<= c_0 n
    $
    as required.
]

Another way to deal with ammortization is using something called the "Piggy Bank method" which allows us to do such analysis in an easier way. In comming up with the above proof, we had to take care of what to choose as $c_0$ as well as deal with the induction that can be much more complicated for weirder data structures.

#definition(sub : "Piggy Bank Method")[
    We will define this by example.

    Let's say we pay $1\$$ to perform an enqueue and deposit $1 \$$ in the piggy bank for a total cost of $2 \$$. Also, let's say that reversing a list costs $1 \$$ per element moved and $1\$$ to start the process and dequeueing from the top of the queue just costs $1\$$.

    We can see that we can only reverse the number of elements we enqueue, hence, our total cost is $n \$$ at the end of a chain of $n$ operation. That would make the ammortized cost $1\$$.
]

This method is often very useful for performing amortized analysis. The piggy bank is also called a potential function, since it is like potential energy that you can use later. The potential function is the amount of money in the bank. 

In the case above, the potential is equal to the number of elements in the reverse list. Note that it is very important in this analysis to prove that the bank account doesn’t go negative. Otherwise, if the bank account can slowly drift off to negative infinity, the whole proof breaks down.

We will see ammortization again when dealing with more complicated data structures, and in that case the piggy bank method would serve us better.

=== The True Queue
The problem still is that we are only promised $O(1)$ ammortized, can we get a $O(1)$ worst case? The answer is yes, but it will take three lists.#footnote[
    We are genuinely surprised how many Haskell (and general functional programming) sources make no mention of this further efficiency, given that it is known since 1995. This is also exactly how the haskell's `Queue` package was implemented before it's deprication.

    The queue equivalent now is `Sequence` which is a doubly linked list (list but both the front and back are accessible and recursable on in $O(1)$). Basically, it has the same relation to queue as list has with stack.
]

The biggest barrier to all this is the reversing. What if we reverse incrementally? That would imply that we never want the front of the list to be empty. That would mean, waiting for it to be empty to do something which is not worth it...

What if instead, we have the rule that the front of the list is always as large as the rear? In this case, every enqueue or dequeue we will need to maintain this property. We will make this accumulator a suffix list which is, as the name suggests, a suffix of the front list. What we want is `length suffix = length front - length rear = drop (length rear) front`.

Whenever enqueue a new element to the rear, we drop the first element of the suffix if it is not empty or start the rotation process if it is empty. Similerly, whenever we dequeue a new element, we drop the first element of the suffix if it is not empty or start the rotatation process if it is. The implementation looks like:
```
data TrueQueue a = Queue [a] [a] [a] deriving Show

empty :: TrueQueue a -> Bool
empty (Queue f r s) = null f && null r

enqueue :: a -> TrueQueue a -> TrueQueue a
enqueue a (Queue f r s) = makeEq f (a:r) s

dequeue :: TrueQueue a -> (a, TrueQueue a)
dequeue (Queue (f:fs) r s) = (f, makeEq fs r s)

peek :: TrueQueue a -> a
peek (Queue (f:fs) _ _) = f

makeEq :: [a] -> [a] -> [a] -> TrueQueue a
makeEq f r [] = let l = rotate f r [] in Queue l [] l
makeEq f r (s:ss) = Queue f r ss

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (r:rs) acc = r:acc -- Front being empty would imply that the rear only has a single element.
rotate (f:fs) (r:rs) acc = f : (rotate fs rs (r:acc)) -- We start the reversing
```

Everything is $O(1)$ due to lazy evaluation. Recall that due to lazy evaluation, we don't complete `rotate` at one go, we do steps as and when required.

Again, this is an extremely neat idea and quite non-trivial. We recommend you internalize it and convince yourself of all the properties before moving ahead.

We will do one problem using queues before moving ahead. Also, another problem is provided for your practice and enjoyment.

#exercise(sub : "Priest and Thieves")[]

#exercise(sub:"A Lagged Fibonacci Sequence (Project Euler 248)")[
A sequence is defined as:
$
g_k = 1 quad "for" 0 <= k <= 1999
g_k = g_(k-2000) + g_(k - 1999) "for" k >= 2000
$
Find $g_k mod 20092010$ for $k = 10^18$.
]

= 





// - Queue
// - Segment Tree
// - BST
// - Set
// - Map
// - Trie
// - Array
// - Leftist Heap
// - Priority Queues
// - Graph
// Chapter content goes here

// cite
// citation 1
// citation 2 
