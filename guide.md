# Work Breakdown

### ✅ Chapter 1: Function Definitions & Flow Control

- **Topics**: Function definition, pattern matching, recursion, induction, `let`, `where`, `if-then-else` as flow control.
- **Time**: 1 Class + 1 Tutorial  
- **Author**: RSA  
- **Notes**: No polymorphism; fully pen-and-paper before code.

---

### 🛠 Chapter 2: Haskell Setup

- **Topics**: Minimal setup (hopefully using HaskellKISS) for different OS.
- **Time**: 1 Parallel Tutorial  
- **Authors**:  
  - **Windows**: R  
  - **Linux**: S  
  - **macOS**: A  

---

### 🔤 Chapter 3: Basic Datatypes

- **Topics**:  
  - `Bool`, `Int` vs `Integer` in extremely brief terms, `Char` (`ord`, `chr`)  
  - Use of `.` vs `"."`
- **Time**: 1 Class + 1 Tutorial  
- **Author**: A  
- **Notes**: No polymorphism yet.  
- **Assignment**: Number Theory & Logic Ops

---

### 🧮 Chapter 4: Types as Sets

- **Topics**:  
  - Tuples, `Either` as `U`, `(->)` as `A^B`, `::` as `∈`  
  - Currying (concept only), implicit parentheses  
  - Basic set theory concepts
- **Time**: 1 Class + 1 Tutorial  
- **Author**: R  

---

### 📃 Chapter 5: Lists

- **Topics**:  
  - List definition & comprehension  
  - Lists as syntax trees  
  - Operations: `head`, `tail`, `!!`, `elem`, `drop`, `take`, `splitAt`  
  - Merge sort, infinite lists  
  - Code Examples:
    ```haskell
    l = 0 : l
    l n = n : l (n+1)
    l a b = a : l b (a+b)
    ```
- **Time**: 2 Classes + 2 Tutorials  
- **Author**: R  

---

### 🔁 Chapter 6: Polymorphism & Higher Order Functions

- **Topics**:  
  - Intro to polymorphism  
  - Higher-order functions  
  - Operators and functions:
    ```haskell
    ($) :: (a -> b) -> a -> b
    a -> b -> (a, b)
    curry, uncurry
    ```
- **Time**: 1 Class + 2 Tutorials  
- **Author**: S  

---

### 🔃 Chapter 7: Advanced List Operations

- **Topics**:  
  - `map`, `filter`, Cartesian product, first through list comprehension, then explicitly defined
  - Quick sort through list comprehension
  - `zip`, `zipWith`  
  - Folds, scans (with syntax tree understanding)  
  - Miller–Rabin primality test
- **Time**: 2 Classes + 3 Tutorials  
- **Author**: A  

---

### Chapter 8: Precomp Data Structures
- **Topics**:
  - Define recursion in recursive data types
  - Define whatever happened in the [[guide.md#🔤 Chapter 3: Basic Datatypes]] section.
  - Define Nat, List, Tree
  
- **Time**:
- **Author**: A

### 🧵 Chapter 9: Computation as Reduction

- **Topics**:  
  - Reduction-based computation (skip Big O)  
  - Syntax trees, lazy evaluation  
  - Examples:
    - Fibonacci via infinite list
    - Test if the following works:
      ```haskell
      (map recip [-5..]) !! n
      ```
- **Time**: 1 Class + 2 Tutorials  
- **Author**: S  

### Chapter 10: Complexity
- **Topics**:
  - Some Notion of complexity that is pretty theoretical
  
- **Time**:
- **Author**: A

### Chapter 11: Post Comp Data Types

- **Topics**:
  - Queue
  - Segment Trees
  - BST
  - Set
  - Map
  
- **Time**:
- **Author**: A

### Chapter 12: Typeclasses

- **Topics**:
  - Recall Polymorphism
  - `deriving`
  - Under the hood of `deriving`
  - Custom Classes
  
- **Author**: R
- **Time**:

### Chapter 13: Monads
- **Topics**:
  - Functors
  - Applicative Functors?
  - Monads:
	- Complexity Monad 
	- Maybe Monad 
	- IO
	
- **Author**: R
- **Time**: 5 Classes + 5 Tutorials

---

## 🗂 Relevent File Structure
In general, do not make new typst files in the chapter area, just edit the previously existent ones.
If you use a figure, please save it as a seperate .typ or .asy file in figures and import when required.

```bash
haskell-course/
├── Book.typ
├── Box.typ
├── Chapter.typ
├── Code.typ
├── Contents.typ
├── Definition.typ
├── Prelude.typ
├── assignments-typ
│   ├── week01.typ
│   ├── ...
│   └── week16.typ
├── chapters-typ
│   ├── ch01_functions.typ
│   ├── ch02_setup_linux.typ
│   ├── ch02_setup_mac.typ
│   ├── ch02_setup_win.typ
│   ├── ch03_datatypes.typ
│   ├── ch04_tuples.typ
│   ├── ch05_lists.typ
│   ├── ch06_polymorphism.typ
│   ├── ch07_advanced_lists.typ
│   ├── ch08_precomp-datatypes.typ
│   ├── ch09_computation.typ
│   ├── ch10_complexity.typ
│   ├── ch11_postcomp-datatypes.typ
│   ├── ch12_typeclasses.typ
│   ├── ch13_monad.typ
│   └── example_chapter.typ
├── example.typ
├── extra-typ
│   ├── appendix.typ
│   └── preface.typ
├── main.typ
├── solutions-typ
│   ├── week01_sol.typ
│   ├── ...
│   └── week16_sol.typ
```
Tutorials, assignments and solutions refer to tutorial handouts(if needed), class and tutorial assignments and their solution files. Keep them as .typ/.tex files for now, the required .hs/.lhs files will be generated later.

Also, if you need to cite something, cite it at the end of your chapter as a comment starting with cite.
```bash
// cite:
// citation 1
// citation 2
```
I will at some point make a script to compile citations.
OR
We can use https://typst.app/docs/reference/model/cite/ and Hayagriva
