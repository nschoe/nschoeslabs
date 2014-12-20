---
title: Haskell
---

### What is Haskell ?
[Haskell](https://www.haskell.org/haskellwiki/Haskell) is a _functional_ programming language with _strong static typing_. [Alp Mestan](http://www.alpmestan.com/) introduced me to Haskell some years ago and I have been a fan ever since.

### Why Haskell ?
There are several reasons why I love Haskell, let me tell you about some of them ; I will talk about ideas and concepts (things that happen in the mind) rather than programing language theory.

#### Lazyness
Haskell is a lazy language. What does it mean ?  
It means that something (may it be a variable, a function, a list, anything) only gets evaluated when it is needed. Specifically, it means that you can deal with _infinity_. For instance, you can write
``` haskell
let xs = [1,2..]
```
which defines an infinite list. The program will compile **and** execute fine, meaning it will not hang. The reason for that is that we did not use the list, so Haskell did not evaluated it.  
It gets even better! If you create an inifine list and display the first 10 elements, it will still compile and execute fine. This is because since we only needed the first 10 elements, Haskell only evaluated the first 10 elements, and not the rest of the list.

#### Functional
What does it mean that Haskell is functional ?  
If you are looking for a complete definition, let me point you to the [Wikipedia page](https://en.wikipedia.org/wiki/Functional_programming). But basically, I tend to think about functional programming languages (and especially Haskell) roughly the same way I think about Mathematics. So basically, you can apply the same mental schemes in Haskell than you do in Maths.  
In particular, in Haskell, functions are of _higher order_, meaning that they are on the same level than variables: you can pass a function as an argument to another function exactly the same way you would pass a variable.  
Another very useful thing is that you can partially apply a function: by not supplying a function with all its parameters make it another function that you can pass around, and/or apply.

#### Recursivity
In Haskell, there is no `while` or `for` loop. Instead you make extensive use of recursion.  
When I am asked to explain Haskell and the differences with non-functional programming Haskell (like in C or C++), this is what I say:

> In C or C++, you tell the computer how to **do** things, in Haskell, you tell the computer what things **are**.

To clarify things, imagine you want to compute the length of a list. In C, you would make a while loop, and increment a counter as long as you find elements in the list. In other worlds, in C you tell the computer to keep incrementing the counter as long as there are elements in the list.  
In Haskell, you tell the computer that the length of an empty list is 0 and that the length of a non-empty list is 1 + the length of the list from which you removed the first element (called the head).  
It turns out this way of describing programs really fits with how my mind works, and I love it! This seems much more natural, and I like the proximity with Maths (for those who wonder, there is lambda calculus in Haskell).

#### Compacity
Haskell is a very compact language. You can achieve complex operations in only a few lines of code. This is thanks to function currying and the fact that funtions are of higher order.  
Besides, those complex operations are very easy to understand because of the way functions are chained. It is (again) similar to Mathematics with the functions compositions operator.  
A simple example: here is a way to count the number of words in a file, in Haksell.
``` haskell
readFile "/path/to/file" >>= return . length . words
```
That is literally **all**. It works, just like that. In basically one line of Haskell code, you can scan a file, count the number of worlds in it. Pretty neat. Look at how functions are chained together, passing the result from one function as a the input for the next one, and so on. Here,

- `readFile` is a function whose argument is `"path/to/file"`
- the result of this (the content of the file) is passed to the `words` function, which cuts a string into a list of strings (words)
- this list of words is passed to the function `length` which returns the length of the list (number of elements)
- This result is then passed to the function `return` (yes, this is Haskell-specific : `return` is just like any other function, not a keyword, as it is in C).  

Another example, suppose we want to compute the number of times the word "the" appear in a file, again, **one** line of Haskell solves this:

``` haskell
readFile "/path/to/file" >>= return . length . filter ((==) "the") . words
```

And here you are. Anyway, I'm just trying to show that Haskell makes extensive use of function combinaison, and that it reads pretty well, and this is why I love Haskell.

### How / Where to Learn Haskell ?

You will find a lot of ressources on the Web, so I'll be brief.

- There is the excellent [Real World Haskell](http://book.realworldhaskell.org/read/) which you can read online.
- [Learn You a Haskell For Great Good](http://learnyouahaskell.com/) is a valuable piece of information.
- I have to point to the **#haskell** IRC channel. The community is among the nicest and most enthusiast there is. Here you can ask questions and you will be answered, quite rapidly. This is much appreciated!
- Another useful ressource is [Hoogle](https://www.haskell.org/hoogle/) which is, as the name implies, "Google for Haskell". In that search engine, you can search functions by type, by module, etc.

Besides, Haskell is very well supported by Emacs and Sublime Text, you can have nice and handy integration (syntaxt highlighting of course, auto-completion, documentation, type signature, hoogle search, etc)