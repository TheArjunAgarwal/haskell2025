import "Modules/Book.typ"
import "Modules/Box.typ"
import "Modules/Chapter.typ"
import "Modules/Code.typ"
import "Modules/Contents.typ"
import "Modules/Prelude.typ"
#import "Modules/Definition.typ" : def
#import "Modules/Exercise.typ" : exercise


= On Strings and Characters
== Basic String Operations
We will now talk about string operations. As we mentioned in the start, strings are a list of characters. This autmoatically implies a lot of advanced string related operations would need to go through lists. Hence, we will only cover the basic ones here.
```
>>> "hello" ++ " " ++ "world!"
"hello world"
>>> 'h' : "ello world"
"hello world"
>>> 'h' ++ "ello world"
<interactive>:78:1: error: [GHC-83865]
>>> "hello" : " " : "world"
<interactive>:79:17: error: [GHC-83865]
>>> length "hello"
5
>>> reverse "hello"
"olleh"
>>> take 3 "hello"
"hel"
>>> drop 3 "hello"
"lo"
>>> splitAt 3 "hello"
("hel","lo")
```
We want you to observe and infer that `(++)` is used to concat two strings while `(:)` is used to append a character to a string. This distinction matters as doing it any other way creates a horrible error.

`length` provides the length of the string, `reverse` reverses the string and `take` and `drop` allow us to either take the first few elements of a string or dispose of them. One could simply define `splitAt n str = (take n str, drop n str)`.

Note, `(:)` is a primitive and is defined axiomatically. That is, we cannot breakdown it's implementation. It just exists and works. The exact working will become more clearer in chapter 9 and 11.

Defining the other functions is just an excercise in recursion, and a very straightforward one at that. The only one with any cleverness will be  `reverse `, but alas we have already seen it in chapter 1.
```
(++) :: String -> String -> String
[] ++ str = str
(x:xs) ++ ys = x : (xs ++ ys)

length :: String -> Int
length "" = 0
length (x:xs) = 1 + length xs

take :: Int -> String -> Int
take _ "" = ""
take 0 str = str
take n (x:xs) = x : (take (n-1) xs)

drop :: Int -> String -> String
drop _ "" = ""
drop 0 str = str
drop n (x:xs) = drop (n-1) xs

naiveReverse :: String -> String
naiveReverse "" = ""
naiveReverse (x:xs) = (naiveReverse xs) ++ [x]

betterReverse :: String -> String
betterReverse (x:xs) = go (x:xs) [] where
  go [] rev = rev
  go (x:xs) rev = go xs (x:rev)
```
`naiveReverse` clearly uses some $(n(n+1))/2$ append operations where $n$ is the length of the list as we use concat unnecesarily and it is expensive. On the other hand, `betterReverse` usea only some `n` append operations. This makes it much faster and is indeed how `reverse` is defined.

== Dealing with Characters
We will now talk about characters. Haskell packs up all the functions relating to them in a module called `Data.Char`. We will explore some of the functions there.

So if you are following along, feel free to enter `import Data.Char` in your ghci or add it to the top of your haskell file.

The most basic and importent functions here are `ord` and `chr`. Characters, like the ones you are reading now, are represented inside a computer using numbers. These numbers are part of a standard called ASCII (American Standard Code for Information Interchange), or more generally, Unicode.

In Haskell, the function `ord` takes a character and returns its corresponding numeric code (called its code point). The function `chr` does the reverse: it takes a number and returns the character it represents.
```
>>> ord 'g'
103
>>> ord 'G'
71
>>> chr 71
'G'
>>> chr '103
'g'
```
The ASCII standard originally defined 128 characters, numbered from 0 to 127. These include English letters, digits, punctuation, and some control characters that do not represent symbols but serve technical purposes. For example, 

- `'\SOH'` - start of heading
- `'\EOT'` - end of transmission(used in sending data to denote if sending is over. If it is not recived, an error may be percived.)
- `'\ETX'` - end of text
- `'\a'` - alert(sometimes denoted as `'\BEL'`)
- `\n` - new line(sometimes denored as `'\LF'`)
- `'\t'` - horizontal tab(sometimes denored as `'\HT'`)
- `'\SP'` - space(often denoted as `' '`)

While `chr :: Char -> Int` is defined on all valid characters, `ord :: Int -> Char` is not defined on all integers for the reason that we don't have as many characters as integers.

The first defined character, aka `ord 0`, is `\NUL`. It is a control character used to represents the null character or nothing. `ord (-1)` results in an error, as does any other negitive number.

We have in total 34 such control characters. From $0-32$ and then at $127$(`'\DEL'`, used to log the use of delete. Not to be confused with `'\BS'` which is used to log backspaces).

Although the ASCII range ends at 127, because it was designed for a 7-bit system, modern systems use Unicode, which extends this idea and assigns unique numbers to over a million characters - including symbols from nearly every language. As Haskell uses Unicode under the hood. That means ord and chr can go well beyond ASCII:
```
>>> ord '☃'
9731
>>> chr 9731
'☃'
```
We will not go into how `ord` and `chr` are implemented, as that involves lower-level details. Just know that they work reliably and are part of the Haskell standard library.

With this out of the way, we can look at some more char based functions.
```
>>> isLower 'a'
True
>>> isLower 'A'
False
>>> isLower ','
False
>>> isUpper 'a'
False
>>> isUpper 'A'
True
>>> isUpper ','
False

>>> toLower 'a'
'a'
>>> toLower 'A'
'a'
>>> toLower ','
,
>>> toUpper 'a'
'A'
>>> toUpper 'A'
'A'
>>> toUpper ','
,
```
Simmiler functions are `isSpace, isDigit, isAlpha, isAlphaNum` for white spaces(space, tab, newline), digits, alphabets and alphanumerics(alphabets or number).

This means one could simply turn a whole string lower case or filter out only the alphabnumeric characters using `map` and `filter`.
```
>>> map toLower "HelLo WOrlD, I am Mixed uP"
"hello world, i am mixed up"

>>> filter isAlphaNum "mix^@($ed &(u*(!p m&(!^#e)*!^ss"
"mixedupmess"
```

< I will complete this section later. >
// TO - DO
// Rest of Characters
// Cipher(a blue print, with a part of it as excercise)
 
#exercise(subject: "Palindrome?")[
  (i) Given a string of lowercase characters, write a function `paliStr :: String -> Bool` to figure out if it is a palindrome.
  
  (ii) Using the `show` function, write a function `paliInt :: Integer -> Bool` to figure out if a number is palindrome.
]

= A list question
#exercise(subject: "Simpson's Fermat")[
  Fermat's last theorem claims that $x^n + y^n = z^n$ has no solution over integers for integral $n > 2$. After 200 years of work, the proof was finally found by Andrew Wiles in 1998.

  However, in the Simpson's episode titled “The Wizard of Evergreen Terrace”, Homer writes $3987^12 + 4365^12 = 4472^12$. If you put this on your phone calculator, or any handheld calculator, you shall find this to be true.

  This is a prank by one of the show's writer David S. Cohen who wrote a code to find near misses to Fermat's Last Theorem. We want you to find some more such near misses. Write a function `simpsonFermat :: (Int, Int) -> (Int, Int) -> Float -> (Int, Int, Int, Int)` which takes the range for the bases, range for the exponents and an error tolerence(in percentage of distence form the RHS) and returns a tuple containing the satisfying bases and the exponent.
]