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

= Binary Search Tree
A common problem we have is finding things, say integers, in a list. This is quite a task as `elem` is $O(n)$. Similerly, deleting something from a list is also quite costly as we need to find it, delete it and then rejoin the list.

Can we come up with a data structure which could allow us to do this faster? Consider the following data structure
```
data Btree t = Empty | Node t (Btree t) (Btree t)
```

This is called a binary tree. Now consider
#definition(sub : "Binary Search Tree")[
    A binary Search tree is a binary tree with the property:
    - Every left descendant of a node has value less than that node.
    - Every right descendant of a node has value larger than that node.
]
Thus, we can convert a list to a binary Search tree by simply doing almost a version of quick sort
```
lisToTree :: Ord a => [a] -> Btree a
lisToTree []     = Empty
lisToTree (x:xs) =
  Node x (lisToTree [l | l <- xs, l < x])
         (lisToTree [l | l <- xs, l >  x])
```
We can also talk about the height of the tree aka the number of levels in the tree.
```
height :: BTree a -> Integer
height Empty = 0
height (Node a left right) = 1 + max (height left) (height right)
```
This clearly takes time $O(h)$ where $h$ is equal to the height of the tree.

Moving ahead, we can insert and search the tree as follow:
```

insert :: Ord a => a -> Btree a -> Btree a
insert a Empty = Node a Empty Empty
insert a (Node x left right)
  | a == x    = Node x left right
  | a < x     = Node x (insert a left) right
  | otherwise = Node x left (insert a right)

search :: Ord a => a -> Btree a -> Bool
search a Empty = False
search a (Node x left right)
    | a == x    = True
    | a < x     = search a left
    | a > x     = search a right
```
Both these operations clearly take worst case $O(h)$ time where $h$ is the height of the tree. Another, intrsting function is `findMin` and `findMax` which return the minimum and maximum element of the tree.
```
findMin :: Ord a => BTree a -> a
findMin Empty = error "empty tree"
findMin (Node a Empty Empty) = a
findMin (Node a Empty right) = a
findMin (Node a left _) = min a (findMin left)
```
#exercise(sub : "findMax")[
    implement the function `findMax` which find's the maximum element of a binary search tree.
]

It is again easy to see that worst case time complexity is $O(h)$. We are now finally ready to implement the delete.

Deletion is somewhat trickier, because removing a node warrents appropriately linking its two descendant subtrees. We can be faced with three cases:
- If the doomed node is childless, we delete it and replace the void with `Empty`.
- If the doomed node has one child, we just move the child up the hierarchy.
-  If the to-be-deleted node has two children, our solution is to relabel this node with the minimum element in it's right descendants. This works as all elements in the right tree are greater than the deleted node and this is the lowest such node, maintaining the rightness of the right tree.

We implement this as:
```
delete :: Ord a => a -> Btree a -> Btree a
delete a Empty = Empty
delete a (Node x left right)
    |a == x = join left right
    |a > x = Node x left (delete a right)
    |a < x = Node x (delete a left) right

join :: Ord a => Btree a -> Btree a -> Btree a
join Empty Empty = Empty
join Empty r = r
join l Empty = l
join l r = let k = findMin r in Node k l (delete k r)
```
Note, we make 3 $O(h)$ operations. This makes delete also $O(h)$.

This all seems jolly good, the only issue is that based on the initial order inseertions and deletions of the tree, the height could be of $O(n)$ and we end up doing no better.#footnote[From the quick sort analysis, remember the average height of tree is $O(log n)$, but the problem here is the worst case.] 

The fact of the matter is, given $n$ elements, we can make a binary search tree with height $ceil(log_2 n) = ceil(log n)$ which would get all the complexities to $O(log n)$. This property is called balance.

So can we modify our insertion and deletion operators to keep the tree balanced without adding too much of additional complexity? As it turns out, yes.

The main idea is that we can rotate trees by choosing a different node and modifying the given tree to a binary tree. For example

```
Node x (Node y (Leaf a) (Leaf b)) (Leaf c)

-> Node y (Leaf a) (Leaf b) (Node x Empty Leaf c) (not binary, sad)

-> Node y (Leaf a) (Node x (Leaf b) Leaf c)
```

This is useful for say
```
Node 3 (Leaf 2) (Node 5 (Leaf 4) (Node 7 (Leaf 6) (Leaf 8)))
-> Node 5 (Node 3 (Leaf 2) (Leaf 4)) (Node 7 (Leaf 6) (Leaf 8))
```

An auxiliary idea is using a proxy for balence. Balence is a rather hard to mantain quality. Instead, we should choose some quantifiable way to say if a tree is balenced or not. It is especially nice if this proxy happens to be easy to compute and update.

In ourcase, we take the proxy for balence to be the difference in height of left and right trees. Our balence proxy will be that the difference in these heights is atmost $1$. Such trees are called height-balenced trees.

The rotation scheme we will was created by Georgy Adelson-Velsky and Evgenii Landis in 1962 and the subsequent tree is called AVL trees. To aid some of the things we will do, we declare the data as:
```
data AvlTree = Empty | Node t Int (AvlTree t) (AvlTree t)
```
where the `Int` is there to store the height of the tree. We will now define some functions which will make our life easier:

```
height :: AVL t -> Int
height Empty              = -1   -- convention: empty tree has height -1
height (Node _ h _ _)     = h

imbalance :: AVL t -> Int
imbalance Empty          = 0
imbalance (Node _ _ l r) = height l - height r
```

We want `abs imbalance <= 1`.

#exercise(sub : "findMin")[
    Write a function `findMin :: Ord t => AVL t -> t` which finds the minimum element in the AVL tree.
]
#exercise(sub : "mkNode")[
    Write a function `mkNode :: Ord t => t -> AVL t -> AVL t -> AVL t` which takes a node `x` and two trees `tl` and `tr` and makes a tree `Node x h tl tr` with an appropriate value of `h`. 
]

We could have given this as a completely as an excercise, but as it is an important component of the upcomming tree making, we are providing the solution.

#footnote[
    #rotate(180deg)[
```
mkNode :: Ord t => t -> AVL t -> AVL t -> AVL t
mkNode x l r = Node x (1 + max (height l) (height r)) l r
```]
]

Notice that `height, imbalance, mkNode` are all $O(1)$ as we are pattern matching against the definition.

As we want to do everything via rotations, we will now define two rotations, one which takes the left child and makes it the parent (`rotateRight` as the original parent moves to the right) and another that takes the right child and makes it the parent (`rotateLeft`).

We will also setup the notation that, given the initial parent `x`, it's left tree is `tl` and right tree `tr`. If we look into say the left tree, it is of the form `Node lc _ tll tlr` and similerly, the right subtree is of the form `Node rc _ trl trr` where `lc` stands for the left child, `rc` for the right child and the subtrees are `t` followed by the branches we took from `x` to reach there. That is `tll` means subtree after traversing left branch followed by a left branch.

This will allow us to define rotations as:

```
rotateRight :: AVL t -> AVL t
rotateRight (Node x _ (Node lc _ tll tlr) tr) =
  mkNode y tll (mkNode x tlr tr)
rotateRight t = t -- Catchall for trees with no left child

rotateLeft :: AVL t -> AVL t
rotateLeft (Node x _ tl (Node rc _ trl trr)) =
  mkNode rc (mkNode x tl trl) trr
rotateLeft t = t -- Catchall for trees with no right child
```
Notice, we are using the @definition_of_Binary_Search_Tree properties only. We are yet to see what is happening to the balance.

We will now define a function rebalance which given a AVL tree with `abs imbalance == 2`, balances the tree. We only need this specific case as starting with a balanced tree, any operation can only increase the imbalance by $1$ in either direction as an insertion or deletion can increase or decrease height by atmost 1 respectively.

We will consider the following cases
// Will need a proof of correctness...
- `imbalence == 2` and imbalence of the left subtree is `-1` : left rotate the left subtree and then right rotate the tree
- `imbalance == 2` and both subtrees are balanced : rotate the entire tree right
- `imbalence == -2` and both subtrees are balenced :  rotate the entire tree left
- `imbalence == -2` and imbalence of right subtree is `1` : right rotate the right subtree and then left rotate the tree

This leads to
```
balance :: Ord t => AVL t -> AVL t
balance t@(Node x _ tl tr)
  | bf == 2 && bfl >= 0             = rotateRight t
  | bf == 2                         = rotateRight (mkNode x (rotateLeft tl) tr)
  | bf == -2 && bfr <= 0            = rotateLeft t
  | bf == -2                        = rotateLeft (mkNode x tl (rotateRight tr))
  | otherwise = mkNode x tl tr
  where 
  (bf, bfl, bfr) = (imbalance t, imbalance tl, imbalance tr)
balance t = t
```
Notice that rebalance is $O(1)$ as we are just moving things around via pattern matching.

We can now finally define `insert`, `contains` and `delete`

```
insert :: Ord t => AVL t -> t -> AVL t
insert Empty x = Node x 0 Empty Empty
insert (Node y _ l r) x
  | x < y     = balance (mkNode y (insert l x) r)
  | x > y     = balance (mkNode y l (insert r x))
  | otherwise = Node y (height (Node y (height l) l r)) l r  -- no duplicates


search :: Ord t => AVL t -> t -> Bool
search Empty _ = False
search (Node y _ l r) x
  | x < y     = search l x
  | x > y     = search r x
  | otherwise = True

delete :: Ord t => AVL t -> t -> AVL t
delete Empty _ = Empty
delete (Node y _ l r) x
  | x < y     = balance (mkNode y (delete l x) r)
  | x > y     = balance (mkNode y l (delete r x))
  | otherwise =
      case r of
        Empty -> l
        _     -> let s = smallest r
                 in balance (mkNode s l (delete r s))
```
One cam clearly see that we only make 1 balencing per `insert` or `delete` and hence, still mantain the $O(h) = O(log n)$ complexity. 

We will not see any specific problems using the `BTree` or `AvlTree`, these are commonly used as precursors to more powerful data structures.

#exercise(sub : "Traversals and Sort")[
    Write a function `traverse :: Ord a => AvlTree a -> [a]` which traverses the tree genrating a increasing function. 

    Write a function `listToAVL :: Ord a => [a] -> AvlTree a` which converts a list to an AVL tree.

    Write a function `treeSort :: Ord a => [a] -> [a]` which sorts a list using the AVL tree. What is the time complexity of this?
]

= Sets and Maps
=== Sets
#definition(sub : "Sets")[
    A set is a data structure that can store unique values, without any particular order.
]
A set normally has the operations `search`, `insert` and `delete`. We shall also show that these operations are enough to implement `union` and `intersection`. Often, `empty` and `isEmpty` are also provided to create an empty set and check if a set is empty respectively.

#exercise(sub : "Naive List Set")[
    Complete the following implementation of set using lists.
    ```
    newtype Set a = Setof [a]
    
    empty :: Set a
    isEmpty :: Set a -> Bool
    search :: Set a -> a -> Bool
    insert :: Set a -> a -> Set a
    delete :: Set a -> a -> Set a
    ```
]

Implementing `union` and `inter` is quite easy and direct. 
```
union :: Eq a => Set a -> Set a -> Set a
union (Setof xs) (Setof []) = Setof xs
union (Setof xs) (Setof (y:ys) = union (insert (Setof xs) y) (Setof ys)

intersect :: Eq a => Set a -> Set a -> Set a
intersect (Setof xs) (Setof ys) = Setof [y | y <- ys, elem y xs]
```

However, as we might notice, these are quite slow as `search`, `insert` and `delete` are $O(n)$ due to needing to check if the element is unique. 

We can circumvent this by instead using an AVL tree as the underlying data structure. Note, this imposes the need for the elements to have the property `Ord`. While we can circumvent it by using more general trees, doing so is beyond the scope of this book.

```
import qualified AVLTree as AVL 
-- This allows us to distinguish between the AVL Tree functions and the ones we define here, given we package AVL tree as a module.
newtype Set a = Setof (AVLTree a)

empty = Setof Nil
isEmpty (Setof t) = t == Empty
search (Setof t) x = AVL.search t x
insert (Setof t) x = Setof (AVL.insert t x)
delete (Setof t) x = Setof (AVL.delete t x)
```

Notice, `search`, `insert` and `delete` are $O(log(n))$. Translating the definition of `union` and `intersect` from above is somewhat tricky. The recursive list structure did make the process quite a breeze. So what if we bring it back?

```
treeToLis :: AVLTree a -> [a]
treeToLis Empty = []
treeToLis (Node x _ lt rt) = (treeToLis lt) ++ [x] ++ (treeToLis rt) 
```

The above algorihtm clearly runs in $O(n log n)$. We can optimize to do so in $O(n)$, the idea being something we have used quite a few times.
```
treeToLis :: AVLTree a -> [a]
treeToLis t = go t [] where
    go Empty l = l
    go (Node x _ lt rt) l = go lt (x: go rt l)
```

We will now just combine these sorted lists.
```
unionMerge :: Ord a => [a] -> [a] -> [a]
unionMerge xs [] = xs
unionMerge [] ys = ys
unionMerge (x:xs) (y:ys)
    |x == y = x: (unionMerge xs ys)
    |x < y  = x : y : (unionMerge xs ys)
    |x > y  = y : x : (unionMerge xs ys) 

union :: Set a -> Set a -> Set a
union (Setof t1) (Setof t2) = Setof t where
    t = unionMerge (treeToLis t1) (treeToLis t2)
```
Notice, `unionMerge` is $O(m+n)$ where $m,n$ is the length of lists. This makes `union` $O(m + n)$ where $m,n$ are the cardinalities of the sets.

#exercise(sub: "Intersect")[
    Write `interMerge` and `intersection` to implement intersection of sets in $O(m+n)$ time where $m,n$ are the cardinalities of the sets.
]

Set is often the underlying data structure for a lot of more advanced data structures. You will see some of them in the excercise.

=== Maps
#def(sub: "Map")[
3
]


= Maybe Trie (not sure really)

= Hash Based Data structures

// Chapter content goes here

= Exercise
// - Leftist Heap
// - Priority Queues
#exercise(sub : "Old Driver Trees")[
Design and Implement a data structure to maintain an array of length $n$, supporting the following operations:
1. `add`: Add $x$ to the value in the interval $[l, r]$.
2. `cover`: Set the value in the interval $[l, r]$ to $x$.
3. `sum`: Calculate the sum of the values ​​in the interval $[l, r]$.

You are given that the types of operations, $l$ and $r$ are independent and uniformly random within their respective ranges.

We want the expected complexity to be linear (proving this is rather difficult although).

Hint: Given the array $a_1, a_2, dots, a_n$, We want to mantain a set of $(l,r,x)$ such that $a_l, a_(l+1), dots, a_r$ are equal to $x$. Can you take it from here? These are called Old Driver Trees or Chtholly trees.
]

#exercise(sub : "Color the Axis (Luogo P1840)")[
There are $n$ points on a number line, located at $1, 2, dots, n$. Initially, all points are colored black. Then we perform $m$ operations. In the $i$-th operation, all points in the interval $[l_i, r_i]$ (inclusive) are colored white. After each operation, output the number of points that are still black.

Implement a function `white :: Int -> [(Int, Int)] -> [Int]` which takes $n$ and the operations as a list of form $(l_i, r_i)$ and returns a list of outputs at every step.

*Example*

```
white 10 [(3,3),(5,7),(2,8)] = [9,6,3]
```
]

#exercise(sub : "Willem, Chtholly and Seniorious (Codeforces 897E)")[
After years of wear and tear, Seniorious the sword is in bad condition. Willem is following an old manual to try to restore it. 

The sword can be reprasented by a line of $n$ talisman pieces, each with an integer value $a_i$.

To restore it, Willem must perform $m$ operations of the following $4$ types:
+ `1 l r x` - For all $i$ in $[l, r]$, set $a_i = a_i + x$.
+ `2 l r x` - For all $i$ in $[l, r]$, set $a_i = x$.
+ `3 l r x` - Output the $x$-th smallest element among $a_l, a_(l+1), dots, a_r$.
+ `4 l r x` - Output the $sum_(i in [l,r]) a_i^x$.

Write a function `restore :: [Int] -> [(Int, Int, Int, Int)] -> [Int]` which takes in the talisman values, the list of instructions and gives the outputs produced in the order they are produced in.
]



// cite
// citation 1
// citation 2 
