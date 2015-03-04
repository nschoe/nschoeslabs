---
title: Overview of Common A.I. Algorithms and Their Application To the N-Queens Problem - Part I
description: In this article, we will see four general-purpose A.I. algorithms (W, X, Y and Z). We will understand how they work in the background, what they are usually used for and how we can implement them to solve the problem of the N-Queens.
toc: yes
tags: ai,algorithms,haskell
---

## Introduction
Hey!  
Welcome here! Ready for action?  
Today I'll like us to work on some classic, general-purpose and well-known Artificial Intelligence (A.I.) algorithms:

- Hill-climbing (or Steepest Ascent)
- Simulated Annealing
- Local Beam Search
- \(A) Genetic Algorithm

You may know them, you may have used them, you might have heard of them, and very often, that'll be just that: _you have heard of them_. And I was the same: I _knew_ all of them, I knew the basic idea behind them (which is fine, for most things, it is important to have a basic understanding, it makes it all the more easier when you decide to learn it in a bit more thorough way) but I had never **played/worked with them**.  
And this is what we are going to do here today!  
Sounds coold, doesn't it?

For each of these algorithms, we will understand what the main idea about them is. Then we will try to pursue a little further by trying to pinpoint the specifics: enumerate the steps it is composed of, their order and their importance.  
Because we understand better that way we will then code an actual implementation, in Haskell (as always, you know I love it, can't help it!), so that we'll be able to **actually** play with them, and boost our understanding.  
Merely coding an implementation would not be enough to play and visualize results, so we will apply these algorithms to a common problem in A.I.: the N-Queens Problem (more in this a few lines below).

![](/images/warning.png) I don't pretend I have _mastered_ any of these algorithms. I am not here to give a thorough and full review of each of these algorithms.  
What happens is that based on dozens of articles, books and papers on A.I. that I read, I caught terms, jargon, algorithms names, and many many interesting notions. But after some retrospective, I noticed that I had not **experimented** with much of the notions, concepts and algorithms. **I felt I was not the only one** in this case, so I decided to write this article: I aim to provide the reader with a **concrete**, **factual** and **usable** implementation of these algorithms and a test-case. **That-is-all**.  
I will be **delighted** to hear comments (good and bad) about my approach, my implementation or my explanations!

### Why The Hell Do I Do That?
Well, as I have just mentionned, I wanted to give anybody the opportunity to _easily_ experiment with A.I. and in particular with these algorithms. You have to know that _there is nothing new here_ for the A.I. field. I haven't invented these algorithms: they are all well-known by any A.I. student. But this is it: _by the A.I. students_.  
It is amazing what a little A.I. might do in almost any application, and I just want everybody to know that some low-level A.I. is achievable, not boring and accessible.

I'd like for you to think about this series of articles as a go-to ressource for when you are decided to take on an A.I. project and you feel like getting an introduction or a quick refresher course.

### Where Doest That Come From?

The theme of this article (_i.e._ the choice of four algorithms and the idea to implement them on the N-Queens problem) comes from what I consider my bible: [Artificial Intelligence, A Modern Approach](http://www.amazon.com/Artificial-Intelligence-Modern-Approach-Edition/dp/0136042597) (2nd Edition), by [Stuart Russel](http://www.cs.berkeley.edu/~russell/) and [Peter Norvig](http://www.norvig.com/).  
This book is a must-have, really (though now the 3rd Edition is out, so go get that one!) as it explains concepts, ideas and gives a point of reference in a lot of fields in A.I.  
They don't give implementations. And this is the genius of Norvig & Russel: they explain the concept, the inner workings so well, that you can derive an implementation easily enough. I would like to thanks them for this wonderful piece of ~~art~~ work.

### What is That N-Queens Thing? Are You Talking Poker?
The [N-Queen Puzzle](https://wikipedia.org/en/Eight_queens_puzzle), initially presented as the 8 Queens Puzzle is a puzzle, or problem in which you are requested to place 8 queens on chess board, without any of the queens attacking themselves.  
This problem is interesting because it a very good candidate to test the scalability of an algorithm: a chess board is an 8 x 8 grid, so it is very easy to extend the problem of placing N queens on a N x N board. This way, one can easily see which of the algorithms scale better when you go from 8 x 8 board to 100 x 100, 1000 x 1000 or even a million by a million squares!

![An example of a solution, from Wikipedia](/images/8_queens_example.png "This is the only symmetric solution for the 8 queens problem.")

![](/images/warning.png) I'd like to highlight the fact that the goal of this article is **not** to provide an answer to the N-Queens problem. Just by reading the Wikipedia article, you will find some ressources that show algorithms and methods used already (there is even a Python code all ready for you).  
In this article, we don't really care about the solution in itself, what we want to focus on is make an **Artificial Intelligence** that finds a solution to solve the problem. Mind the difference.

### Let's Go or What?
Yeah, let's go! I hope you'll enjoy the ride !  

Let me just introduce some basic terminology, so that we are all on the same page, and then, I promise, the fun begins.   
In Artificial Intelligence, you will (or _have_) hear about _states_. When we want to formalize a situation, to run some algorithms, we use the concept of states. Most formally, **a state describes the current _state_ of the world**. Now, that's said, and that means.... well nothing.  
And everything.  
It all depends on one **strong** concept (the strongest of all, I shall say): **abstraction**.  
How would you describe the world around us? I think the only exact answer to that question would be to be able to know the position, velocity, acceleration, etc of all particules in the whole world. But apparently the quantuum physicists say it's impossible...  
Back on track, the point I was trying to make is that in order to have efficient algorithms, you need to make the correct abstractions to describe the problem. Here are a few examples, in case you need clarification:

- if you are designing an algorithms to play chess on a computer, the state (the description) is the position of all 32 pieces on the board. You don't need to include the room temperature, the humidity level, the color of the walls, etc.
- Note that if you are designing a chess player **robot**, with hands and fingers grasping the pieces along the board, now that changes everything: you _might_ want to monitor the humidity so that you can adjust your grip on the pieces or monitor the color of the walls to calibrate the camera and be able to distinguish the chess pieces for example

Well all of this to say that we will often use the term "state" and for us, it will mean "the world". It should then be obvious to you that in our case, the N-Queens, the state will be the position of the queens on the board.  
A _successor function_ (or simply a successor) is a function that takes a state as a parameter and returns one or several other states that can be obtained from the given state by applying a _strategy_. Now what is that strategy?  
Well... this **is** Artificial Intelligence. This part of the so-called "algorithms". For a quick example, if your strategy is of type "breadth-first search" it means you will develop all child nodes of a parent node before goind any further, if your strategy is of type "depth-first search", well it is sort of the converse: you will try to go as deep as possible before developing another child.  
Now that was just the idea, we will have plenty of time to cover that more thoroughly!

How will our algorithms, or agents actually _make decisions_? One of the ways is to use a _cost function_. Quickly said, a cost function gives a price, or a cost: an estimation of _how bad_ a chosen path is.  
Okay that was not so clear after all. Let me say it again: a cost function will tell you how much it will cost you to go down the path you're currently examining. The goal is, obviously to minimize the overall cost. (Now, if you think it's weird to reason in terms of "how bad" your solution will be, you can make the cost function become an _evaluation function_ which will tell you how good your solution is; the goal would be to pick the highest. Actually make yourselv familiar with that as I will endlessly switch between cost functions and objective functions, so sometimes we will try to minimize a function, sometimes maximize it. But we are always talking about the same thing. Be ready for that).  
To make an analogy (again and ever: analogies are the keys to understanding concepts), imagine we are building a GPS application, you want to go from town A to town B. There are several ways. You cost function could be the distance, if your goal is to find the shortest path. It might be the time, if your goal is to minimize the time you spend on the road. Or it might be a combinaison of both: `cost(dist,time) = a * dist + b * time`, adjusting the coefficients. And a great many things depend on finding the _good_ cost function.

Okay, boring part over, we-are-ready!

This will probably make your brain hurt a little, so go feed it some energy (white chocolate is my pick here) and get relax (I suggest [Archive, Again](https://www.youtube.com/watch?v=iofT_kc7FH8)).  
And let's go make some A.I.!

![](/images/warning.png)Oh I forgot to tell you: unless you already know that stuff, I suggest you read the articles in the order. There is a reason why I chose to talk about these algorithms in that order: they are _sort of_ "evolution" one to another, and I will introduce some notions along the way. So better to read in the correct order.  
See you in the next section!

![](/images/warning.png)**Disclaimer**  
I am not sure yet of the public my website will reach, as it is all new. As such, I will tend to explain _a lot_, both A.I. (topic of the article) **and** Haskell (because I want people to _really_ find it easy to study the algorithms and their implementations).  
But maybe all of you will be quite familiar with Haskell or (for those who are not) you will find Haskell literal enough that you don't need (or want?) the additional Haskell explanations. In which case, feel free to skip the parts explaining Haskell (usually, this is the paragraph after each code block) and [**tell me**](mailto:ns.schoe@gmail.com) so that I stop doing that in upcoming articles.  
I don't want to bore or annoy you :-)

## The Hill-climbing algorithm
Okay my friends, welcome in the first part of this article!  
The Hill-climbing, or steepest-ascent algorithm is part of the _Informed Searching Algorithms_, which means they are algorithms that make use of _additional information_ compared to the raw problem definition.  
Now what does that mean exactly?

Before I answer this, let me say something: 

> When you are wondering what something means or implies in Artificial Intelligence (for your Agent); just ask what it would mean **for you**.

Even though Artificial Intelligence is not necessarily trying to mimick the human mind and the human intelligence, it still provides a strong reference point.  
Okay so, what would "additional information" mean for us compared to the problem definition? Otherwise, what is likely to _help_ you with respect to the problem?

Imagine your problem is the one we talked about before: going from town A to B, minimizing, say the time. Bham, that's the definition of the problem. Good luck with that.  
You should know that _uninformed algorithms_ perform actually pretty bad. And that's quite normal: with no other information than the problem's definition, there is only so much you can do.  
So if you want to find the quickest path to go from A to B, what is likely to help us is... a map! It would be even better if we knew the distance between the towns, if the roads are highways, two-lanes, small roads, etc. With this, our agent can start doing things better than just going blindly random.

Since we are going to apply our algorithms to the N-Queens problem, it is worth noting that the N-Queens problem is part of those problems where only the solution matters, not the path to the solution. Just to make sure you caught the difference:

- in the [Traveling Salesman Problem](http://www.wikipedia.org/en/Travelling_salesman_problem), the goal is to visit all N towns while minimizing the total traveled distance. The end of the problem will be that we had visited all N towns, here what we are interested in (what we will optimize) is _the way_ (the order) in which we will visit these N towns.
- in chess, the goal is to win, to check & mate the adversary's king. It doesn't matter if we first moved the pawns or the knights. All it matters is to win. We want to optimize our moves, but we will do so only to win, not for the sake of having "better" moves.
- in the N-Queens problem, the goal is to have 8 Queens, so that no two attack each other. It doesn't matter if we placed two queens at a time, or one by one, etc.

Alright, I will stop with the examples, now.

### Yeah Yeah, Whatever. What Does it Have to do With a Hill Anyway?
Ah... well... it seems it needs clarification.  
If you are thinking that kind of hill...

![](/images/hill_xp.jpg "Wrong kind of hill, sorry!")

... then you are wrong. Our hill looks more like...

![Image from _Artificial Intelligence, A Mordern Approach_](/images/hill_ai.jpg "Yeah, that suits our needs better")

So if you hadn't guessed it by now, our hill-climbing algorithm will climb the hill of our cost function, or more exactly our objective function.  
The graph below represents the value of the objective function for each state. Here the horizontal axis is continuous, but let's pretend it is discrete. Along the axis, we will have our states for the N-Queens, that is for each configuration of the queens, we compute an objective value, whose value is reported on the vertical axis.

## The Implementation

### Some Clarifications
The first algorithm we are going to take a look at is also the simplest.  
First of all, we need a heuristic.

#### What is a Heuristic And Which Are We going to Use?
I find it quite difficult to _define_ a heuristic. I will try my best, as usual, to **give you a feeling** of the idea behind it.  
Put simply, a heuristic is something that will help us solve our problem.  
Yes thank you sir, that was useless...  
Now you're being harsh. Let's try again.  
You can think of a heuristic as an _indicator_. A heuristic will give you a clue of where you are in your algorithm, it will give you an _estimate_ of your current success. If you want to summarize a heuristic, you can say this:

> A heuristic is a function that gives you quickly and easily a _not-too-bad_ (yet most likely wrong) answer

A question somtimes asked to MBA students is to estimate the number of windows in all the buildings in Manhattan. Clearly you have no clue. And if you actually _had_ to give the correct answer, it would take you insanely huge amount of time and ressources. So to answer the question, you will use a form of heuristic. You may start by estimating the total number of buildings in Manhattan, then estimmate the average number of windows for each building. That gives you an estimate. It _is not_ the right answer, of course. But it gives you a _pretty good_ (I prefer myself say a _pretty not-too-bad_ estimate).  
Heuristics are not limited to order of magnitude, though. Keep reading papers on A.I. and you will quickly find better insights.

A last example, that might talk to you. The A* (pronounce "A-star") algorithm is one of the best-known A.I. algorithms today. It is used to find the shortest path between two nodes in a graph. It is **the** example of algorithms using a heuristic.  
A*'s heuristic is usually presented as such: $f(n) = g(n) + h(n)$.  
Here, $f$ is the heuristic. For each node $n$, $g(n)$ is the cost to reach $n$ from the start and $h(n)$ is the _estimated_ cost of the less expensive path to reach a goal node, from the node $n$.  
All in one, $f(n)$ is the estimated cost of the less expensive path that goes through the node $n$.

You might want to take a moment to read that last paragraph again, process the information thoroughly. I'll grab a coffee in the meantime.

![](/images/coffee_break.png "Ho c'mon, that was fun, make a pause in the reading. Enjoy :-)")

Back with me?  
So, now that we know a heuristic is a function that gives us an estimation of the cost of the problem, you might be wondering...

What are we going to use for a heuristic?  
"That, [my friend,] is the right question" (bonus point for those who get the reference).  
From the nature of the problem (placing queens so that they don't attack each other), one "obvious" heuristic is the number of pairs of queens attacking each other. We will use that heuristic to make modifications on our state.

#### Two Big Classes of Problem Formulation
If you re-read the last sentence it ended with "[...] to make modifications on our state". Behind that simple statement is hidden something big: problem formulation.  
We denote two main ways of formulating a problem: _incremental_ or _complete_ formulation. Let's examine the difference with the N-Queens problem:

- in the incremental formulation, the initial state is empty (no queens on the board). The successfor function will then generate a state in which you add a queen on the board. You'll do that until you placed all of your N queens and they don't attack each other. It is called "incremental" because each call to a successor adds a new queen.
- in the complete formulation, you generate a (preferably random, but it doesn't _have to_ be) state in which there are already 8 queens on the board. Each call to the successor function moves a queen to generate a new state.

For some problems, like the N-Queens problem, both formulations actually make sense. You might very well prefer adding one queen at a time, making sure it doesn't collide with any previously-placed queen, until you managed to place them all. Or you might as well have a "starting" idea, you place your 8 queens on the board and... _crap_ it almost works... but only almost: those two ~~bitches~~ queens at the extremities attack one another (this is actually very likely to happen with the 8-queens problem, everybody I showed this problem (including me o_O) to started like "yeah okay, place a queen a bit like a diagonal but with one extra space on it and.... of course the last one is boggus, see image below").

![Typical strategy people firstly think of](/images/8_queens_almost_solution.png "You almost got it, pal.")

(This image was taken again from _Artificial Intelligence, A Modern Approach_)

For the rest of this article, we will use the complete formulation problem, because it is more appropriate in this case, or at least for the four algorithms that we will cover together. So we will generate a random initial state (random is good in A.I., you'll see soon) and we will try to move the queens as we go along.

#### I Remember You Were Taling About "Informed Algorithms"... What is The Additional Information Already?
Well glad you asked!  
Actually I was _just about_ to tell you about it!

For the N-Queens problems, as you might have guessed, a _map_ is not likely to help us. To understand what can help us and in what measure, let's take again, for a second, the incremental problem formulation and apply it to the **8**-Queens problem.  
You start with an empty state, a'right, got that. Easy.  
Then, place first queen, how many possibilities? 64, easy. Yup.  
Then the second? $63$, okay you got the point.  
For the 8-Queens problem (which is, **small**) we have $\frac{64!}{(64-8)!}=\frac{64!}{56!}=1.8\times10^{14}$ possibilites! That's one hell of a lot possibilites. And we're only talking about an $8\times8$ board here. The raw number of possibilites is $\frac{(N^2)!}{(N^2-N)!}$ and if you are interested, [here](http://www.wolframalpha.com/input/?i=plot+%28%28x^2%29!%2F%28x^2-x%29!%29+from+8+to+20) is a plot of the function, just for $N\in[8,20]$. That is pretty steep.

But as you might have guessed this is pretty dumb to think like that. When you placed your first queen, you know that it eliminated every squares on the same row and on the same column. So you have less places in which to fit the second queen, and so on.  
And here is the additional knowledge that we will use: you can place only one queen per column. And this is how we should be reasonning to add our queens incrementally on the board. First place a queen on the left-most column (A). Then add a queen on column B, then C, etc.

Now I said we were going to use the complete problem formulation, how does it translate? We will generate 8 random positions for the 8 queens, in their column. From there one, we will move the queens on a different square **in their column**, trying to reach the configuration where no queens attack any other.  
Just by adding the information that we will use one queen per column, we limited our number of states to $8^8$ which is a little less than 17 million. But that is still $N^N$ and this scales dreadfully bad.

### Principle of the Algorithm
So how is it going to work, how are we going to proceed?  
The principle in itself is quite simple, here are the rough steps:

1. We generate a random (complete) initial state, in which N queens are (randomly) positionned on their respective column. (_Remember: starting now, we will always stay with one queen per column_).
2. We evaluate this state with our cost (or objective) function according to our heuristic, _i.e._ we compute how many pairs of queens attack each other.
3. We generate all children states thanks to the successor function and evaluate each of these states. At that point, we could trace the "hill", _i.e._ the value of our heuristic for each of our states.
4. We select the state with the lowest heuristic value (thus the best objective value), which is better than our current value, and make the corresponding move. This becomes our new state.
5. We repeat steps 3 and 4 until we reach a solution (_i.e._ we have no two queens attacking each other)

That's the _basic_ principle. Seems fair enough, right?

#### Well... I Think I've Spotted Some Issues...
And you are right. Let's make a quick review...  
Generating a random (initial or not) state can sometimes be a real problem in itself. Here it is basically just a matter of generating $N$ numbers in the interval $[0..N-1]$.  
Then we talked about evaluating our state. From the look of it, this is an operation that we will need to do many many times. So we need to find an efficient way to do so.  
Another thing we will do many times is generating the children states (or _resulting_ states). Let's take a minute to think about it. In our algorithm, we will make one move at a time, _i.e._ move one queen in its column. How many resulting states do we have for a particular state?   
We have $N$ queens on the board, and $N$ squares by column ($N-1$ of which are available) resulting in $N * (N-1)$ resulting states. That is $8*7=56$ states for our 8-Queens problem. This is manageable for the 8-Queens version, but you will notice this function is in $O(N^2)$ which _bad_ in algorithm.

Just a quick reminder for those who need it, $O(1)$ is perfect, $O(log (N))$ is (very) good, this is what we hope to reach in general, $O(N)$ is "the default": what our mind commonly think and the easiest to visualize, and everything above is _bad_: $O(N^2), O(N^3), ...$ are _not good_, $O(e^N)$ is awful, $O(N!) is _terrible_, $O(N^N)$ is, like, infeasible.  
So we _might_ have a problem here with children state generation. We will still do it, though; we will look for enhancements later.  

Looks like we're good to go? Well, hardly. The next step is to select the best state among our generated states, _i.e._ the one with the best objective value _and which must be better than ours_. Nothing garantees us that given our current state, there exists a move that will decrease the number of pairs of queens attacking each other. This is called a **local extrema** in our objective function (or cost function). From now on I may be more likely to talk about objective function rather that cost function, because the algorithm is called "hill-climbing", so in our mind we think about "going higher", _.i.e._ move toward states with higher value; it corresponds to a _lowest_ value of our heuristic, though. Just keep that in mind so you don't mix the concepts.  
On the next figure, I have highlighted a local maxima:

![Local maxima: the resulting states have all _lower_ value that the current](/images/hill_ai_local_maxima.jpg)

In this case, our algorithm will be trapped: when it will evaluate the resulting states, it will find that their have lower objective value, yet our current state is not the global maxima. You will see that this is a common problem in A.I. But we'll still go with that for the moment.

Another thing we should be worried of are _paliers_. They are a set of resulting states with all the same values as the current one. Which means their value is, according to our objective function, no better nor worse. The figure below shows a palier:

![A palier, states with the same objective value](/images/hill_ai_palier.jpg)

Last problem we may have: we stated that we should continue doing this until we reached a goal. But again, what if there are no better state than ours? We could have our algorithm run forever, and we don't want that. _As a first shot at this_, we will stop when there is no better state that our current state.

Well... it seems we are good to go! Let's do some Haskell now!

### Basic Version
So first things first: let's make this _work_ before we _tweak_ it.  
As usual, all the code is available [here](https://github.com/nschoe/common-ai-n-queens), so you don't have to worry about it, just read and follow :-)

#### Imports And Cosmetics
First, a list of imports:

```haskell
module Main(main) where

import Control.Monad (forM_, forever, when)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Data.List (sort, intersperse, (\\), sortBy)
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import System.Random (randomRs, newStdGen)
```

Then comes a little cosmetics, nothing of importance, nothing concerning A.I., as I said, it's just cosmetics, so don't fret about it:

```haskell
-- Current version
version :: String
version = "0.2"

-- Char to display an empty square on the board
emptySquare :: Char
emptySquare = '*'

-- Char to display a sqaure on which there are a queen
queenSquare :: Char
queenSquare = 'Q'
```

I just like to keep the version to display it and (if needed) refer to it, and the two "square" char is just for convenience, when you want to display the board, whatever you feels is the most readable.

Okay, so the real thing begins here. First, we define our data type to hold our board. At first, I had began something with a `Vector` of `Vector`s that contained a custom data type which read `data Square = Empty | Queen`. But that meant having always in memory the $N^2$ squares, which can quickly fill up the memory.  
Then I thought that what _actually_ mattered were the positions of the queens on the board, _not_ the empty squares.

I finally came up with the following data type:

```haskell
{-
Our data type to represent a N x N board.
After all, all that matter is the size of the board and the position of the queens (if any)
For efficiency's sake, we will keep the list of coordinates sorted
-}
data Board = Board {
    boardDim    :: Int              -- N (dimension of the board)
  , boardQueens :: [(Int, Int)]     -- List of (x, y) coordinates for the queens (origin is at top left)
}
```

It contains the dimension and a list of $(x,y)$ coordinates that represent the queens' positions. Please note that the origin is considered at the top left corner, the first coordinate is the **row number** and the second, well obviously the **column number**. Oh and don't forget that we start counting from 0.

#### Our Data Type: Representation And Display

Haskell doesn't implement [dependent types](https://wikipedia.org/en/Dependent_type); so to ensure our data makes sense (_i.e._ prevent a coordinate to be "out of the board") we provide a function to create a new `Board`, we never create one manually (for a real project, your would _obviously_ define the data type in a separate module, and not export its constructor):

```haskell
{-
Since there are some constraints (on the coordinates w.r.t. the board's dimension), we provide a way to construct a new
board, that checks the coordinates and ensures queens are not added out-of-bound.
-}
createBoard :: Int -> [(Int, Int)] -> Board
createBoard dim xs = Board dim validQueens
    where isQueenOnBoard n (x, y) = x < n && y < n
          validQueens = sort $ filter (isQueenOnBoard dim) xs
```

I think this is pretty self-explanatory: from the list of coordinates the user gives us, we filter out those who are out-of-bound. Simple.  
You will note that we sort the list, the order relation for pairs is to sort them first according their first coordinate, then their second. This is why I decided that the first coordinate would be the row number and that our origin point would be the at top left.

As with every articles I will write on this website, it is about _getting a feeling_ about things, _understand_ how things work and assemble together. And as my mathematics Prépa School teacher always said: 

> Always draw things when you can

we will want a way to visualize our board (well, at least in small dimension, I don't see how we can draw a 100 x 100 board in the console). Let me first show you the code, and I'll explain right after:

```haskell
{-
To display a board, here is the idea:
    * we create one string of length N x N, containing only the empty character
    * we traverse the list of coordinates, compute the offset in the string and replace the empty by a queen
    * then we display on screen, breaking the string in strings of length N
-}
displayBoard :: Board -> IO ()
displayBoard (Board n xs) = do
    let str = take (n^2) (repeat emptySquare)
        oneLineCoordinates = map computeOffset xs
        withQueensReplaced = replaceQueens oneLineCoordinates str
        formatted          = breakEveryN withQueensReplaced
    putStrLn formatted
    where computeOffset (x, y) = x * n + y

          replaceQueens xs zs = replaceQueens' (-1) xs zs

          replaceQueens' _ [] zs     = zs
          replaceQueens' currN (y:ys) zs = take (y - currN - 1) zs ++ queenSquare : replaceQueens' y ys (drop (y - currN) zs)

          breakEveryN [] = []
          breakEveryN xs = intersperse ' ' (take n xs) ++ '\n' : breakEveryN (drop n xs)
```

If you read the comments, you should have the basic idea how to draw the board on the console.  
The thing is, we only have the positions of the queens, but we want to draw empty squares too, if we want an accurate visual. So here's how I saw it:

1. We first generate an empty board, consisting of $N^2$ empty squares with `let str = take (n^2) (repeat emptySquare)`
2. Then we will "replace" the corresponding squares with the queens display characters. For now the string is one linear $N^2$-long string while our coordinates are formatted along $(x,y)$ pairs. We need to convert these form of coordinates into a linear one. We do this with `oneLineCoordinates = map computeOffset xs`, `computeOffset` is a very simple function.
3. What we have now is our same list of coordinates, but transformed into offset that can be related to the one-line, linear string representing our (current empty) board. The nice thing is that, with our **sorted** list of pair-formatted coordinates, we will get the list of linear-formatted coordinates **also sorted**, and this is very handy for the next function call: inserting the queens at the right place.  
`withQueensReplaced = replaceQueens oneLineCoordinates str` achieves that. Let's dive a bit into that function, because it is a bit tricky (though fairly simple in itself):  
We use a common pattern in Haskell when your function calls a version of itself (often named after itself, with a prime (`'`) added at the end when it needs an accumulator. Accumulators are very common in Haskell, either directly like here or through the use of a `fold` function. Anyway, here is the idea.  
We have a list of (**sorted**, this is important) coordinates and a list of characters representing empty squares. The idea is to traverse our list of coordinates (which are offsets now, remember), keep the correct amount of empty characters, insert our queen, discard an empty character (which was just replaced by a queen) and continue on moving in the list of empty characters with out next queen to insert. This is easy.  
But the "trick" lies in the fact that our offset-style coordinates are offset... from the **beginning** of the list of squares. And this means the correct values only holds for the very first queen. For each and every queen that follows, we need to subtract our current position from the offset. This is what our accumulator is for: keep track of the current offset we are at. That's all.  
The base case is obvious: when we don't have any more queens to insert, we return the rest of the (empty) characters.  
The recursive case works exactly like we just said. `y` represents the offset (from the beginning of the string) and `currN` the current offset we are at, then `take (y - currN - 1) zs` ensures we take the right number of empty characters before inserting our queen, which we do with `++ queenSquare`.  
Now we took care of inserting the current queen, all is left to do now is call recursion, _provided we give the correct parameters_: our new offset is simply the offset of the last queen we inserted, thus `y`; the remaining list of queens to insert is bound to `ys` and we need to discard `y - currN` of the empty characters we had on the list.  
Please note that the `(:)` and `(++)` operators are both **right** infixed, with the same priority, this is why `[list] ++ queenSquare : [list]` work, because it will be interpreted as `[list] ++ (queenSquare : [list]) = [list] ++ ([queenSquare:list])` which is indeed, the concatenation `(++)` of two lists.
4. Anyway, now that we have our correctly-offsetted queens, to display an actual _grid_ or _board_ we just break that N-long string into N N-long strings. Yeah, read that again, there is no mistakes: we do want N strings, each of length N. Got it? ^^  
Because vertical spacing is larger that horizontal spaces, the grid would appear like this:

```zsh
****
****
****
****
```

This is why we insert spaces between each character (with `intersperse ' '`) so that it looks more like this :

```zsh
* * * *
* * * *
* * * *
* * * *
```

We have enough to test this in our console, just to check that everything displays nicely: 

```haskell
main :: IO ()
main = do
    let board = createBoard 8 [(4,0), (5,1), (6,2), (3,3), (4,4), (5,5), (6,6), (5,7)]
    displayBoard board
```

and we got this on our console:

```zsh
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * Q * * * *
Q * * * Q * * *
* Q * * * Q * Q
* * Q * * * Q *
* * * * * * * *
```

Pretty neat! Isn't it?  
Ho c'mon! I know this isn't A.I. _yet_, but cheer up!

#### Program Architecture
Just before we dive into full-blown A.I., let me just show you how I have actually architectured the program. Since we will want to test and compare our different algorithms, I put the whole thing into a menu-style program. When launched, it asks you what you want to do (_e.g._ generate a new board, which algorithm to run on the generated board, issue how many random tests to compare performances, etc).  
Nothing fancy here: just display the menu, ask for input, parse input, if this matches to something we recognize, execute corresponding function then loop, otherwise insult user and loop.  
We will keep some program state into a `IORef` (which is a mutable IO variable) to store things such as the generated board, the stats from last run, etc. You will see as it goes, for the moment here is the architecture:

```haskell
-- Entry point, present the menu and wait for input
main :: IO ()
main = do
    greetings
    currentBoard <- newIORef Nothing
    forever (displayMenu >> handleInput currentBoard)

-- Just some greetings + the current version
greetings :: IO ()
greetings = do
    let str = "..::|| LET'S HAVE FUN WITH THE QUEENS! ||::.."
        str2 = take (length str) (repeat '=')
        str3 = "Welcome to the A.I. algorithms comparison v" ++ version
    forM_ [str, str2, str3] putStrLn
```

So the entry point just writes "hello" on the console, create an empty `IORef` and then loop onto "asking for input, processing input".

```haskell
-- Present the user with the list of possible actions
displayMenu :: IO ()
displayMenu = do
    let str = "Here are the current supported actions:"
        menu = ["r8:\tGenerate random 8x8 board."
              , "hill\tSolve board with hill climbing algorithm"
              , "q:\tExit Program."
               ]
    putStrLn str
    forM_ menu (putStrLn . (:) '\t')
```

Nothing fancy here, we define a list of strings to present the user with the possibilites, and display them.

```haskell
-- Take input from stdin, issue action (if matching)
handleInput :: IORef (Maybe Board) -> IO ()
handleInput board = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case input of
        "r8"      -> generateRandomN board 8
        "hill"    -> hillClimbing board
        "q"       -> exitProgram
        otherwise -> wrongInput board

-- Oh I'm sure you'll recognize that...
wrongInput :: IORef (Maybe Board) -> IO ()
wrongInput board =
    putStrLn "I'm sorry, my responses are limited. You must are the right question.\n"
    >> displayMenu
    >> handleInput board

-- ... and that too :-)
exitProgram :: IO ()
exitProgram =
    putStrLn "Program. Terminated."
    >> exitSuccess
```

Nothing too complicated here. For the moment, for simplicity's sake, we only present the possibility of generating an $8\times8$ board, we will add scaling later. The `hill` command will run our hill-climbing algorithm on the generated board.

![](/images/warning.png)Don't try to copy / paste the code as you read the article. To make it compatible with a project-based approach, the samples of code that I present you all along the article correspond to what I'm writing about it. It does not correspond to the final code (the idea behind is obviously the same, but there are small architectural changes that will make the code not compile).  
So I'd advise you to read the extract of codes alon with the explanations. If you are interested in the code, please go to the [github repo](https://github.com/nschoe/common-ai-n-queens) where lies the final code, which compiles.  
Reading the article will help you digest the code better.

#### Generating a Random Board
Okay, so this is it!  A.I. stuff!  
Remember we said we would begin with a random state? Well now's the time to do that.  
Here is how we will do it (simple, really): remember we used the additional information that only one queen can be present in a column, so basically all we have to do is generate N random positions, in the range $[0,N-1]$ (I hope you had not forget the 0 thing...). The first generated position will be the row position for the queen in the first column, the second will be the row position for the queen in the second column, all the way to the N-th column.

Here is our function:

```haskell
-- Generates a random board
generateRandomN :: IORef (Maybe Board) -> Int -> IO ()
generateRandomN board n = do
    randomOffsets <- newStdGen >>= return . randomRs (0,n-1) :: IO [Int]
    let randomPositions = zip randomOffsets [0..(n-1)]
        newBoard = createBoard n randomPositions
    writeIORef board (Just newBoard)
    putStrLn "New board generated"
    when (n <= 20) (displayBoard newBoard)
```

The `Int` parameter is obviously the board dimension. Note that our function will generate a new board and store it in the `IORef`, erasing any previous board, without asking for confirmation.  
We first generate randoms offsets in the correct range. Remember the first coordinate corresponds to the _row number_, this is why we then use `let randomPositions = zip randomOffsets [0..(n-1)]` to pack those random offsets as the first coordinates of our $(x,y)$ pairs.  
Then it's a matter of calling the constructor function `createBoard` with the dimension and the positions and storing it in the `IORef` for persistence. When we generated a board that is of "displayable" size (I chose $20\times20$ as a limit here), we display it.  
Let's try it by generating some $8\times8$ boards!

```zsh
Here are the current supported actions:
    r8: Generate random 8x8 board.
    hill    Solve board with hill climbing algorithm
    q:  Exit Program.
    d:  Debug
> r8
New board generated
* Q Q * * * * Q
* * * * * * * *
Q * * * * * * *
* * * * * * * *
* * * * * Q * *
* * * * * * * *
* * * Q Q * * *
* * * * * * Q *
```

And another example for the fun:

```zsh
Here are the current supported actions:
    r8: Generate random 8x8 board.
    hill    Solve board with hill climbing algorithm
    q:  Exit Program.
    d:  Debug
> r8
New board generated
* * * * * * * *
* * * * * * Q *
Q Q * * * * * *
* * * * * Q * *
* * * * * * * Q
* * * * * * * *
* * * Q Q * * *
* * Q * * * * *
```

O-kay! We got our first step checked. Time for a break!

![](/images/coffee_break.png "Brace yourselves, the good stuff is coming!")

#### Generate All Successors
The next big thing we have established we should do is generate all successors from our current state, so that we can compute their respective values and choose the best (provided it is better than our current state). That proved to be a bit tricky, anyway here is the function, I'll comment after:

```haskell
-- Generate *ALL* successors (be careful of size)
generateAllSuccessors :: Board -> [Board]
generateAllSuccessors board@(Board n xs) =
    concat $ for xs (\pair -> extrudeColumn pair (xs \\ [pair]))

    where
        for = flip map
        extrudeColumn (row,col) cs =
            let thatCol   = [(x,y) | x <- [0..(n-1)] \\ [row], y <- [col]]
                appended  = zipWith (:) thatCol (repeat cs)
                newBoards = map (createBoard n) appended
            in newBoards
```

For the type signature it obvious: from the `Board`, we wil generate a list `[Board]`: our resulting states.  Here is how I thought about it: we think about our board as columns, in which a queen is placed. A resulting state differs from our current state by **one** queen, being move inside its column. So the tough work is to generate the resulting states of one column, then it is a simple matter of applying that function to all columns.  
This is what I did: the `extrudeColumn` generates all resulting states for one column (hence, $N-1$ ($7$ in our case) states). `extrudeColumn` will then return a list of boards `[Board]`.  
Hence the little piece of code in from of it, which basically apply `extrudeColumn` to all $N$ columns (but that yields a list of lists of boards `[[Board]]`, which is why `concat` is called).

So we need to focus on `extrudeColumn`. I have decided that we should pass it two parameters: the current column we have extruding and the other remaining colums. This is why I use a `map` (well actually a `map` whose parameters I flipped, then calling it `for`, as an obvious reference to `forM` / `mapM`, functions from the `Control.Monad` module). `extrudeColumn` will then be called as many times as there are columns, each time giving it the current column. For the resulting colums, I simply use list subtraction `xs \\ [pair]` which returns the list `xs` whose element `pair` has been removed.

Now we enter the execution of `extrudeColumn`. Remember only the current column will change for all $N-1$ states that we will generate for this column. Our queen is located at coordinates $(x,y)$ which can be rewritten `(rowNumber, colNumber)`. **All other $(x,y)$ pairs will star the same**.  
So what we do is generate all the $(x,y)$ pairs for **the current column** (which is $N$ pairs) and we make sure to remove the pair that corresponds to our current position (we don't want to generate the same state as the current state): hence $N-1$ pairs.  
I'm not sure it was all clear, so let me take an example. Suppose we are dealing with the first (left-most) column (so the $y$ coordinate will be $0$). Let's say the queen in this column is located at index 4. Our tuple is then $(4,0)$. Right?  
What we want is to generate all possibles moves for that queen on column $0$. These possibles moves are: $(0,0),(1,0),(2,0),(3,0),(5,0),(6,0),(7,0)$ (note that we removed $(4,0)$ because this is our current state).  
Okay, I think you got that. We achieve that by using Haskell's list generation notation: `let thatCol = [(x,y) | x <- [0..(n-1)] \\ [row], y <- [col]]`. That line can be read as this:  

- "Generate a list..." (`[` and `]` delimiters)
- ...of all pairs `(x,y)`... (`(x,y)`)
- ...such that x takes all values from $0$ to $n-1$ except the current value "row"... (`| x <- [0..(n-1)] \\ [row]`)
- ...and y takes the only value `col` (`, y <- [col]`)

Now what we have is only a list of positions for a single queen (in the being-processed column). We need to create a board from each of these queen positions. For that, it is a simple matter of appending _all the other queens' position_, like this: `appended  = zipWith (:) thatCol (repeat cs)`.  
`zipWith` is presented with a list containing our newly-generated positions for our queen, the list of all the other queens position, and simply 'reunite' the two to form only one list containig N positions (because we have N queens).  
Just to be sure this is clear, what is bound to `appended` is a list of lists of N coordinates for which only the first coordinate differ (we are operating on **one column**).  
Then it is just a matter of calling `createBoard` on all those lists to actually create the boards, which we return.  
Remember this is then `concat`ed to generate all states.  
I think this is time we ran a test. We will generate all states and count them. How many are we supposed to get?  
We have $N$ rows, for each of these rows, we generate $N-1$ new states, so we should be having $N\times(N-1)=8*7=56$ states in our example. Let's try it:

```haskell
main :: IO ()
main = do
    currentBoard <- newIORef Nothing
    generateRandomN currentBoard 8
    (Just newBoard) <- readIORef currentBoard
    let successors = generateAllSuccessors newBoard
    print (length successors)
```

(Bear with me for the dangerous binding, this is just to test our function, we will discard this code right after)
And we get: 

```zsh
New board generated
* Q * * * * Q *
* * * * Q Q * *
* * * Q * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * Q * * * * Q
Q * * * * * * *

56
```

We did get 56 states generated. Cool!  
Not bad, we are almost done. All there is left now is run the hill-climbing algorithm.

#### What About Our Objective Function?
Yeah you're right, we'd better cover this right now, as it will be needed.

Let's make a quick recap on what we have so far:

- We have studied our algorithm and listed the steps (and their order) that we will need
- We have covered our implementation: how we designed the data type, how we compute on it, etc
- We have implemented our successor function

What we need to cover now is our evaluation function: how do we evaluate a given state, _i.e._ how do we count the number of attacking pairs. I will walk you through how I came to design the function rather than bulk-give it to you, it just makes more sense.

Our objective function, if you recall, is the number of attacking pairs or queens. Well that seems easy enough. Well...

```zsh
* Q * * Q *
* * * * * *
* * * * * *
* * * * * *
* * * * * *
* * * * * *
```

The above example is quite trivial indeed: two queens are attacking, so that makes one pair. Same deal if the queens were aligned vertically. But we can have diagonal attacks too:

```zsh
* Q * * Q *
* * * * * *
* * * Q * *
* * * * * *
* * * * * *
* * * * * *
```

Now we have two attacking pairs. But don't forget we can have a queen attacking several other queens too:

```zsh
* Q * * Q *
* * * * * *
* * * * Q *
* * * * * *
* * * * * *
* * * * * *
```

Here we have three pairs of attacking queens.

Ok, back to logic. The first function I implemented (but we won't actually use it; I am still giving it to you though, because this was the basis of my thinking) is a function that tells us if the board is valid (no queens attacking each other) or not. Here is how I thought about it:  

- we know that there can be only one queen per column, what that means _mathematically_? It means, no two queens can have the same $y$ coordinate. Good.  
Well, actually, this is the same for the rows: if we have two queens on the same row, they will have the same $x$ coordinate.
- So we just made one easy check: I will call this the orthogonal checks. Let's first go with that. As you might have guessed, we will use an accumulator: we traverse the list of coordinates and we store the coordinates we encounter. At each step we check if the coordinate we are checking is in the list of "already-met" coordinates. Simple.

```haskell
isBoardValid :: Board -> Bool
isBoardValid (Board n xs) =
    let orthogonalCheck  = checkRowAndCols xs
    in orthogonalCheck

    where checkRowAndCols = checkRowAndCols' [] []

          checkRowAndCols' _ _ []           = True
          checkRowAndCols' xs ys ((x,y):cs) | x `elem` xs || y `elem` ys = False
                                            | otherwise                  = checkRowAndCols' (x:xs) (y:ys) cs
```

This is text-book recursion: if we reach the case where we don't have any coordinates to check, then the board is valid. The recursion case is just like we said: given a pair of coordinates `(x,y)`, we check whether `x` or `y` has already been found on another queen (these `x`s coordinates are stored in `xs`, likewise for `y`s). If we do find a match, then stop and return `False`, otherwise, we store both `x` and `y` in our lists and we recurse against the rest of the coordinates.

Simple test case first:

```haskell
main :: IO ()
main = do
    let board = createBoard 8 [(0,1), (0,4)]
    displayBoard board
    print (isBoardValid board)
```

We get:

```zsh
* Q * * Q * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *

False
```

So far so good, I'll let you check that it works on vertically aligned queens as weel. Now, let's try this:

```haskell
main :: IO ()
main = do
    let board = createBoard 8 [(0,1), (3,4)]
    displayBoard board
    print (isBoardValid board)
```

```zsh
* Q * * * * * *
* * * * * * * *
* * * * * * * *
* * * * Q * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *

True
```

This was expected: so far we did not check the diagonal cases. And this is the tricky part: how do we check for diagonal alignment? The first idea that might pop to mind is take a pair of coodinates, check it against all other pairs of coodinates, and do that for all. This _will_ work, but we traverse the remainig list of coordinates $N-1$ times for each of the $N$ pairs we examine. So the algorithm is in $O(N^2)$ and... this is not good. Won't be good when we scale that to $1000\times1000$. Surely we can do better.    
So far we managed to check for orthogonal alignment is $O(N)$: we traverse the list just once. If only there were a way to do the same for the diagonals. Well actually there is!  
As I was thinking about this, a little sentence popped-up in my mind "take the problem under another angle". People always tells you to do that, but it never _actually_ works. Well this time, I wasn't convinced either, but as a joke I rotated the piece of paper on which I had drawn the board. And this is funny because that actually triggered it: now that I rotated $45^{\circ}$, the queens that were previously diagonally aligned were "orthogonally" aligned, if you see what I mean. And that was it: we just needed to rotate the map by $45^{\circ}$!  
For those who don't come from a mathematical background (or if you forgot) a rotation matrix is of the form (in 2D, obviously)

$$\begin{bmatrix}
\cos(\theta) & -\sin(\theta) \\
\sin(\theta) & \cos(\theta)
\end{bmatrix}$$

And here since our $\theta$ is $45^{\circ}$ or $^{\pi}/_{4}$, our rotation matrix is:

$$\frac{\sqrt{2}}{2} .
\begin{bmatrix}
1 & -1 \\
1 & 1
\end{bmatrix}$$

But we don't care about the $\frac{\sqrt{2}}{2}$ coefficient, this is just for normalization. We will just keep the matrix and thus, let's compute our new coordinates:

$$
\begin{bmatrix}
1 & -1 \\
1 & 1
\end{bmatrix}
.
\begin{bmatrix}
x\\
y
\end{bmatrix}
=
\begin{bmatrix}
x-y\\
x+y
\end{bmatrix}
$$

And voilà! We have our new set of coordinates!  
This **is** big deal: once we computed our new set of coordinates (in a new base), we can now perform the same "orthogonal" checks. We just have to check if two queens are aligned verticall or horizontally in our new set of coordinates. Let's just modify our function:

```haskell
isBoardValid :: Board -> Bool
isBoardValid (Board n xs) =
    let orthogonalCheck  = checkRowAndCols xs
        newCoordinates   = map rotate45 xs
        diagonalCheck    = checkRowAndCols newCoordinates
    in orthogonalCheck && diagonalCheck

    where checkRowAndCols = checkRowAndCols' [] []

          checkRowAndCols' _ _ []           = True
          checkRowAndCols' xs ys ((x,y):cs) | x `elem` xs || y `elem` ys = False
                                            | otherwise                  = checkRowAndCols' (x:xs) (y:ys) cs
          
          rotate45 (x, y) =
            let x' = x - y
                y' = x + y
            in (x', y')
```

The new part consists of applying `rotate45` to all pairs of coordinates and then just check these new coordinates the same way we checked for vertical and horizontal alignment.  
And this is much better, because now we traverse the list 3 times: once for checking horiztonal and vertical alignment, once for changing the coordinates and once again to check for horizontal and vertical alignment in the new base. This is $O(3\times N)=O(N)$.  
Of course, this function has been written to follow our logic, but surely we can manage to traverse the list only once by using 4 lists: we check for alignment in the first base, rotate the coordinates and check for alignment in the second base, just in one step. But this is hardly as readable. (Of course, for serious use, you should use it).

Time for a check: 

```zsh
* Q * * * * * *
* * * * * * * *
* * * * * * * *
* * * * Q * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *

False
```

Oh yeah!

Well, that was the easy part: we only check whether or not a board was valid. It doesn't give us much more information. We need to count how many queens are attacking each other. We will use the same base, obviously.  
The idea is the following:  

- first, we will run the function in the entire list, because for now, it stopped as soon as it found a queen attacking another one.
- we will use the same recursion with accumulator, but we will add another accumulator: the number of attacking pairs we have found for now.
- the checks are performed as follows:
    + if the `x` or `y` coordinate of the pair we are currently evaluating has already been found, it means that queen is either horizontally or vertically aligned with another, so we increase by one the number of attacking pairs.
    + if **both** `x` and `y` coordinates have already been found, it means our current queen is **both** attacking a queen vertically **and** horizontally (the bold should tell you that I first missed it and wondered why I had not the correct results!), so we increase the number of attacking pairs **by two**
    + otherwise, it means the queen is not attacking any other queen.

Let's code this: 

```haskell
countAttackingPairs :: Board -> Int
countAttackingPairs (Board n xs) =
    let orthogonalAttacks  = checkRowsAndCols xs
        rotatedCoordinates = map rotate45 xs
        diagonalAttacks    = checkRowsAndCols rotatedCoordinates
    in (orthogonalAttacks + diagonalAttacks)

    where   checkRowsAndCols = checkRowsAndCols' 0 [] []

            checkRowsAndCols' m _ _ [] = m
            checkRowsAndCols' m xs ys ((x,y):cs) | x `elem` xs && y `elem` ys = checkRowsAndCols' (m+2) xs ys cs
                                                | x `elem` xs = checkRowsAndCols' (m+1) xs (y:ys) cs
                                                | y `elem` ys = checkRowsAndCols' (m+1) (x:xs) ys cs
                                                | otherwise   = checkRowsAndCols' m (x:xs) (y:ys) cs

            rotate45 (x,y) =
                let x' = x - y
                    y' = x + y
                in (x',y')
```

Most of the function you already know. Let's review `checkRowsAndCols`, you can see the new accumulator `m`. The base case, when we have examined all coordinates, we simply return `m`, the number of attacking pairs we have computed.  
The recursion cases, we have talked about (adding $+1$ when only $x$ or $y$ was found, adding $+2$ when both of them were found). Be sure to pay attention to the way we add new coordinates, for instance in `| x ``elem`` xs = checkRowsAndCols' (m+1) xs (y:ys) cs`. In this case, `x` (only) was found in the coordinates we had already encountered, so not only we add $+1$ to `m`, but this is `y` that we add to the list (well, obvious: `x` was already in it, but this is easy to mix the cases when typing).  

Well, this is all, let's try it:

```haskell
main :: IO ()
main = do
    let board = createBoard 8 [(0,1), (0, 2)]
    displayBoard board
    print (countAttackingPairs board)
```

We get:

```zsh
* Q Q * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *

1
```

Good. Now let's try this one:

```zsh
* Q Q * * * * Q
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *

2
```

This is good stuff: three queens aligned indeed make **two** pairs: the middle one with the left (one pair) and the middle one with the right one (one pair). But let's see why we don't have "three" output, because as far as we know, the left-most queen and the right-most queen **are** aligned, so our algorithm _should_ have matched them as attacking.  
But if you read our algorithm again, the _first time_ it sees a queen, it will not have any stored `x` or `y`, so it will just add that `x` and `y` into the list, but won't increment `m`. Only the second time will it increase `m`. So our design automatically solved it for us!

We can check that it works with diagonal queens:

```haskell
* Q Q * * * * Q
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * *
* * * * * * * Q
* * * * * * * *

4
```

And more importantly, that the queen at the bottom **is** indeed registered to attack to other queens. So we got a correct 4 here.

#### The Hill-Climbing Steps
All is left to us is put it all together. We have built every piece of code we need, now we need to apply them in the correct order, make the necessary checks and loop.

First, the "top-level" function:

```haskell
hillClimbing :: IORef (Maybe Board) -> IO ()
hillClimbing board = do
    board' <- readIORef board
    case board' of
        Nothing     -> putStrLn "No board has been generated yet."
        Just b      -> do
            let currentValue = countAttackingPairs b
            if (currentValue == 0) then do
                putStrLn "No need for algorithm, the board is already solved:"
                displayBoard b
            else do
                hillClimbingStep (b, currentValue) board
```

Our algorithm will deal with the generated board, that should be stored in the `IORef`. The "top-level" function makes sure that this `IORef` is not empty and if not, it checks if we don't already have a solved puzzle. Then, it calls `hillClimbingStep`. The name 'step' should indicate you that this is the part of the function that will loop.  
To simplify the implementation, the stepping function will be called not just with the board, but with the board along with its value, this is why we pass it `(b, currentValue) :: (Board, Int)`.

Okay, now the steps.  

```haskell
hillClimbingStep :: (Board, Int) -> IORef (Maybe Board) -> IO ()
hillClimbingStep (stepBoard, stepValue) board = do
    let successors = generateAllSuccessors stepBoard
```

First thing we do is generate all successors of our current board. Now we have a list of states `[Board]`.  

```haskell
        withCost = map (\board -> (board, countAttackingPairs board)) successors
```

Then we need to compute the value of each state, so just call (with `map`) our `countAttackingPairs` function on each `Board`, and we pack it up in with its corresponding board in a pair.

```haskell
        sorted = sortBy (\p1 p2 -> compare (snd p1) (snd p2)) withCost
```

We are interested in the best one, so we sort this list by comparing the value.

```haskell
        best = head sorted
```

And we isolate the first (hence the best) state.  
I am well aware that the code is very step-by-step detailed, and that it impacts performance (no need to generate all successors if it happens one has already no attacking pairs, no need to sort the final list, only taking the best is enough), but what is important is that you get the idea. Plus we will improve our algorithm later, when we all have the idea of the inner workings.

Okay, now we have isolated our best child state, _i.e._ the best possible move we could make, given our previous state. There are 4 possibilities now:

1. Our best state is a solution (no attacking queens):

```haskell
if (snd best == 0) then do
        putStrLn "Solution found:\n"
        writeIORef board (Just . fst $ best)
        when (boardDim (fst best) <= 20) (displayBoard (fst best))
```

Well it's good, we found a solution. Just store that solution in the `IORef` and display it on the console (if it is not too big).

2. Our best state is not a solution, but better that our current (less attacking queens)

```haskell
else if (snd best < stepValue) then do
        putStrLn "Looping on better state..."
        hillClimbingStep best board
```

Then we loop: we call ourselve, with our current board being the best child we just generated.

3. Our best state is worse that our current state, which means that any move we make will create more attacking pairs of queens...

```haskell
else if (snd best > stepValue) then do
        putStrLn "Algorithm stopped because only worse states result from this one:"
        when (boardDim stepBoard <= 20) (displayBoard stepBoard)
```

Well it's not good, we stop the algorithm, saying we are stuck and display it (if not too big) the best solution we have so far. **This, my friends, is a local maxima**. It means we have reached a point when **a single move** can't make the situation better. What we need, in order to get out of there, is to make several _apparent_ worse moves, to _eventually_ reach a better state.  
This is **extremely difficult** for computer, as they don't see _the bigger picture_, welcome to the real A.I.  
Of course, we will deal with that later. Don't be too depressed about it and stay a bit longer with me.

4. Well the only case left is that our best state has the same value of our current.

```haskell
else do
        putStrLn $ "Algorithm stopped because the best resulting state has the same value as the current one (" ++ show stepValue ++ ")"
```

We stop the algorithm too, telling the user we don't find a better move to make. This corresponds to a palier of the objective function.

#### Time For Tests

Now that we have got all pieces plugged together, it is time to do some manual tests, for feedback and satisfaction. Let's generate some random boards and run the hill-climbing algorithm on them:

```zsh
Looping on better state...
Looping on better state...
Looping on better state...
Algorithm stopped because the best resulting state has the same value as the current one (1)

Looping on better state...
Looping on better state...
Looping on better state...
Looping on better state...
Algorithm stopped because the best resulting state has the same value as the current one (1)

Looping on better state...
Looping on better state...
Algorithm stopped because the best resulting state has the same value as the current one (1)
```

Okay so it seems very often, we block on one pair of queens attacking themselves, which is not bad, but not the solution.

Sometimes I block on a state where two pairs of queens attack each other:

```zsh
Looping on better state...
Looping on better state...
Looping on better state...
Algorithm stopped because the best resulting state has the same value as the current one (2)
```

And after many tries, I finally got a solution:

```zsh
Looping on better state...
Looping on better state...
Solution found:

* * * * * * Q *
* * * * Q * * *
* * Q * * * * *
Q * * * * * * *
* * * * * Q * *
* * * * * * * Q
* Q * * * * * *
* * * Q * * * *
```

Good! That works!

![](/images/champagne.png)

#### Time For Stats

It's all very good that we have something that works, but as you may have seen, it doesn't work very well: the algorithm gets stuck very often.  
To compare it with others, it would be good to be able to build some sort of stats. We will now modify our functions slightly so that we can measure:

- the percentage of board on which the algorithm succeeded
- the average number of moves our algorithm makes to find a solution
- the average number of moves our algorithm makes to get stuck

How are we going to implement that?  
We will simply create a function that takes our algorithm as a parameter, and the number of tests to run.  
Firstly, we add a line in our menu: 

```haskell
        "stats_hill"    -> runStats hillClimbing board
```

As you have guessed, `runStats` is our function to run the stats, you can see that it takes the `hillClimbing` algorithm as a function. It will call that algorithm on the boars it will generate.  
Let me show you another entry I have added in the menu and one that I modified, you will inderstand right now: `"hill"          -> hillClimbing True board >> return ()` and `"hill_s"        -> hillClimbing False board >> return ()`.

What does this mean?  
The `>> return ()` hints the fact that I have changed the type signature, `hillClimbing` now returns something. We'll cover this in a minute.  
You can see a new argument that we pass to `hillClimbing`, a boolean. It specifies the algorithm if we want it to be verbose or not. Indeed, when you try your algorithm on the console, for one run, you want feedback: it prints its result. But when we are launching a batch of 100,000 runs, we probably don't want the console to be spammed.

Let's see our new implementation:

```haskell
hillClimbing :: Bool -> IORef (Maybe Board) -> IO (Maybe (Bool, Int))
hillClimbing verbose board = do
    board' <- readIORef board
    case board' of
        Nothing     -> do
            when verbose $
                putStrLn "No board has been generated yet."
            return Nothing
        Just b      -> do
            let currentValue = countAttackingPairs b
            if (currentValue == 0) then do
                when verbose $ do
                    putStrLn "No need for algorithm, the board is already solved:"
                    displayBoard b
                return $ Just (True, 0)
            else do
                (succeeded, nbOfSteps) <- hillClimbingStep verbose 0 (b, currentValue) board
                when verbose $ do
                    if succeeded then do
                        putStrLn $ "Found a solution in " ++ show nbOfSteps ++ " steps."
                    else do
                        putStrLn $ "Blocked on a solution after " ++ show nbOfSteps ++ " steps."
                return $ Just (succeeded, nbOfSteps)
```

Two main changes:

- the verbosity argument, as you can see, anything that prints on the console if preceeded by a check on that variable
- the return type is now `IO (Maybe (Bool, Int))`. Why?  
The `(Bool, Int)` returns if the algorithm solved the puzzle or not, and the number of (queen) moves that were made; it will be useful for making statistics.

Likewise, here is the new `hillClimbingStep` implementation:

```haskell
hillClimbingStep :: Bool -> Int -> (Board, Int) -> IORef (Maybe Board) -> IO (Bool, Int)
hillClimbingStep verbose moveNb (stepBoard, stepValue) board = do
    let successors = generateAllSuccessors stepBoard
        withCost = map (\board -> (board, countAttackingPairs board)) successors
        sorted = sortBy (\p1 p2 -> compare (snd p1) (snd p2)) withCost
        best = head sorted
    if (snd best == 0) then do
        writeIORef board (Just . fst $ best)
        when verbose $ do
            putStrLn "Solution found:\n"
            when (boardDim (fst best) <= 20) (displayBoard (fst best))
        return (True, moveNb)
    else if (snd best < stepValue) then do
        when verbose $
            putStrLn "Looping on better state..."
        hillClimbingStep verbose (moveNb + 1) best board
    else if (snd best > stepValue) then do
        when verbose $ do
            putStrLn "Algorithm stopped because only worse states result from this one:"
            when (boardDim stepBoard <= 20) (displayBoard stepBoard)
        return (False, moveNb)
    else do
        when verbose $
            putStrLn $ "Algorithm stopped because the best resulting state has the same value as the current one (" ++ show stepValue ++ ")"
        return (False, moveNb)
```

Compared to our previous implementation, it takes the verbosity and the number of steps it made on that board so far. So that we can know how many steps it took to either solve the puzzle or fail. I won't go into much details, because nothing changed _algorithmic-wise_. We just modified our _implementation_ so that we could gather statistics. Virtually, the only changes we've made here is that when `hillClimbingStep` calls itself for recursion, it increases the `moveNb` and at the end, it returns if it solved the puzzle or not and the number of steps it performed.

Finally, let's see how we implemented the `runStats` function:

```haskell
runStats :: (Bool -> IORef (Maybe Board) -> IO (Maybe (Bool, Int))) -> IORef (Maybe Board) -> IO ()
runStats algo board = do
    -- Ask the user how many steps he wants to run the stats on
    putStr "How many steps do you want to run [100]?\n> " >> hFlush stdout
    rawInput <- getLine
    let howMany = if rawInput == "" then 100 else read rawInput
    -- tId <- forkIO (runStats' algo howMany)
    putStrLn $ "Launching stats on " ++ show howMany ++ " computations."
    runStats' algo howMany

    where
        runStats' algo howMany = do
            (nbSucceeded, stepsToSucceed, stepsToFail) <- runStats'' 0 0 0 algo howMany
            let percentage = fromInteger nbSucceeded / fromInteger howMany * 100 :: Double
                avgOK = fromIntegral stepsToSucceed / fromIntegral nbSucceeded :: Double
                avgKO = fromIntegral stepsToFail / fromIntegral (howMany - nbSucceeded) :: Double
            putStrLn "\n###########################################################"
            putStrLn $ "Solved " ++ show nbSucceeded ++ "/" ++ show howMany ++ " (" ++ take 4 (show percentage) ++ "%)."
            putStrLn $ "It takes an average " ++ show (take 4 (show avgOK)) ++ " steps to find a solution."
            putStrLn $ "It takes an average " ++ show (take 4 (show avgKO)) ++ " steps to fail."
            putStrLn "###########################################################\n"

        runStats'' nbSucceeded stepsToSucceed stepsToFail algo 0 = do
            return (nbSucceeded, stepsToSucceed, stepsToFail)
        runStats'' nbSucceeded stepsToSucceed stepsToFail algo remaining = do
            generateRandomN board 8
            b <- readIORef board
            Just (solved, steps) <- algo False board
            if solved then
                runStats'' (nbSucceeded + 1) (stepsToSucceed + steps) stepsToFail algo (remaining - 1)
            else
                runStats'' nbSucceeded stepsToSucceed (stepsToFail + steps) algo (remaining - 1)
```

From the signature we can see that it takes two parameters: the algorithm to run and our `IORef` to store results in it. So, what is there in this function?

First thing we do is ask how many runs we wish to perform, defaulting to 100 (just hit space for the default choice).  
The actual computations are run in `runStats''`. It generates a random board, run `hillClimbing` on it, then increments the correct variables (`nbSucceeded` and `stepsToSucceed` if the puzzle was solved for instance).

The report is issued in `runStats'`: you can see that we compute the percentage of solved puzzle, the average number of moves it took to solve a puzzle and the average number of moves it took to fail.

Let's run it!

```zsh
> stats_hill
How many steps do you want to run [100]?
> 100000
Launching stats on 100000 computations.

###########################################################
Solved 14009/100000 (14.0%).
It takes an average "3.05" steps to find a solution.
It takes an average "2.98" steps to fail.
###########################################################
```

Okay, so now we have some material. I ran a batch of 100,000 runs (it did took a couple of minutes on my computer). 100,000 is significant enough to draw some interesting conclusions.  
Our basic version is quite bad as you can see: it solves $14\%$ of the puzzles. That's rather limited. This is why we had so much troubles to find a solution when we were running it on hand.  
It is interesting to note that it takes a little over 3 moves to find a solution and a little under 3 moves to fail. So our algorithm doesn't get stuck forever on a board.  
Well, remember that we decided to stop the execution altogether when the best child state is not strictly better than our current state: it ensures we loop too long on a board, but, well... we only solve $14\%$ of the boards.

I haven't ran tests on this, but you surely had a glimpse when I showed you a couple of results earlier: when the algorithm fails, it doesn't fail by a big margin. It doesn't get stuck while there are 4 or 5 pairs of queens attacking each other. A quick sample of individual tests show that most of the time, the failure occurs when there are only pair of queens, sometimes two. This is explained by the fact that our algorithm is said to be **greedy**: at each increment (step), it takes the best generated child state.  
This means two things:  

1. our algorithm is pretty "dumb": it will go straight for the best _apparent_ solution (a bit like if you were in a glass palace, see the solution and run straight to it: you are very likely to bounce off a transparent wall, but in the mean time, you were not far). Greedy algorithms often behave this way: they provide **quick**, but **not optimal** solutions.
2. our algorithm is fast: yes it solves only $14\%$ of cases, but for its defense, when it fails, it leaves us with one (or two) pairs of queens attacking each other _in only 3 moves!_. Don't forget that in the 8-Queens problem, there are about $8^8=17,000,000$ possibilites.

Well, we have made some pretty nice things together; let's say we have a basis.

What do you say we improve that algorithm?  
Yes?  
Cool! Let's pump that percentage up!

### Allowing Lateral Moves

Welcome to our first step toward improvement.  
I don't know if you noticed (while playing with what we have so far on the console) but most of the time, when the algorithm gets stucked, it is not because the best child state it generated is worse that the current state, but _because it has the same value_. Which means it often gets stuck on a palier. And that should tell us something...  
What if the situation was something like this:

![What if we were inches away from a solution?](/images/hill_ai_lateral_move.jpg)

What if we were on that palier (the gray position), and our best child state was the red one (thus with the same value) but was on the path to a solution (the green one)? We _need_ to take that _apparently useless_ step to reach our goal.  
So we should allow the lateral moves (by the way, I'm sure you understood that "lateral moves" referred to the hill curve, it simply means a move with the same value of the current one)?  
Yes! We will try that. But we will take a protection: what if our palier was surrounded by "holes" (child states with worse values)? Then we would loop forever on this palier. And we don't want that, do we?  
So what we are going to do is restrict the number of lateral moves our algorithm can make: every time it makes a lateral move, we will decrement a counter, and if it reaches zero and the best child state is still on a palier, we stop.

How does that sound? Good? Let's do this!

We slightly change the algorithm's signature:

```haskell
hillClimbingStep :: Bool -> Int -> Int -> (Board, Int) -> IORef (Maybe Board) -> IO (Bool, Int)
hillClimbingStep verbose moveNb lateralLimit (stepBoard, stepValue) board = do -- ...
```

When we meet a better state, we just call loop back, as usualy, simply passing the `lateralLimit` untouched (we only decrement the number of lateral moves); the real happens in our `else` case:

```haskell
else do
        if lateralLimit > 0 then do
            when verbose $ do
                putStrLn "Making a lateral move..."
            hillClimbingStep verbose (moveNb + 1) (lateralLimit - 1) best board
        else do
            when verbose $ do
                putStrLn "Algorithm stopped because it reached maximum of lateral moves"
            return (False, moveNb)
```

if we still have authorized lateral moves, we take it!  
Otherwise we stop execution.

#### Some Tests Please!

```zsh
Launching stats on 10000 computations.

###########################################################
Solved 3149/10000 (31.4%).
It takes an average "4.31" steps to find a solution.
It takes an average "100." steps to fail.
###########################################################
```

Well, I could not run 100,000 simulations, it took too much time. As it can possibly spends more time on each board, it takes a lot more time to compute. But we've got results!  
So we see now that we achieved a $31.4\%$ success rate, this is better!  
But there is more:  

- It takes an average $4.31$ moves to solve a puzzle. It was a little over $3$ previously.
- _Besides, it takes $100$ moves to fail_

Those two pieces of information put together give us an important feedback: _we are doing something wrong_. Why?  
Well it takes only about $1.3$ more moves to succeed, right? With no lateral moves, we could solve a puzzle in $3$ moves, now with **100 lateral moves authorized**, it just takes $4.5$ moves to solve a puzzle. Which means we basically made only $1$ lateral move to solve the puzzle.  
What's more, when we fail, we almost always use up our $100$ moves.... This is not right. If we go back to how we choose the new child state (on lateral move), we don't apply any special strategy. *What if there are several moves with the same current value?*. We ignore them.  
I suspect that when we fail, it is because we just bounce between states. Which means that we reach a state when there are only children with the same value, let's call that state $A$. We take the first child state (with the same value), let's call it $B$. Then when we evaluate $B$, we are again on a palier, and we chose $A$... from which we know we will chose $B$. Well do that a hundred times and that is a lot of wasted time (and computer ressource).

We have two solutions:

1. keep track of the last state we were, and do not return in it.
2. when we reach a palier, rather than predictably select the child, we randomly choose a child state _among the children states who have the same value (i.e. are on the palier)_

I'd like to implement the second solution: it is easier to implement, we don't have to carry an extra state with us.

So here we go, the slight modifications are here:

```haskell
hillClimbingStep :: Bool -> Int -> Int -> (Board, Int) -> IORef (Maybe Board) -> IO (Bool, Int)
hillClimbingStep verbose moveNb lateralLimit (stepBoard, stepValue) board = do
    let successors = generateAllSuccessors stepBoard
        withCost = map (\board -> (board, countAttackingPairs board)) successors
        sorted = sortBy (\p1 p2 -> compare (snd p1) (snd p2)) withCost
        best = head sorted
```

I have just written here the beginning of the function, because we will need the `sorted` variable.  
Then the real modifications appear on the last, case, when we are on a palier:

```haskell
else do
        if lateralLimit > 0 then do
            when verbose $ do
                putStrLn "Making a (random) lateral move..."
            let bestOnPalier = takeWhile ((==) stepValue . snd) sorted
            (offset, _) <- newStdGen >>= return . randomR (0, length bestOnPalier - 1)
            hillClimbingStep verbose (moveNb + 1) (lateralLimit - 1) (bestOnPalier !! offset) board
        else do
            when verbose $ do
                putStrLn "Algorithm stopped because it reached maximum of lateral moves"
            return (False, moveNb)
```

So nothing tremendous:  

- `let bestOnPalier = takeWhile ((==) stepValue . snd) sorted` select all states having the same value (which is equal to our current value, remember)
- `(offset, _) <- newStdGen >>= return . randomR (0, length bestOnPalier - 1)` selects a random number between $0$ and $l - 1$ where $l$ is the length of the list that we just generated.
- `(bestOnPalier !! offset)` _actually_ selects the random state form the list

And that's it, we're done!

I think it is time for more tests now, can we do better than $31.4\%$?  
Let's see:

```zsh
Launching stats on 10000 computations.

###########################################################
Solved 9432/10000 (94.3%).
It takes an average "17.8" steps to find a solution.
It takes an average "62.6" steps to fail.
###########################################################
```

Well I it is safe to say we did a pretty good job!  
Look at that: we went from a small $31.4\%$ to a huge $94.3\%$.  
We do notice that it now takes more steps to solve a puzzle, about $18$, and now, it "only" takes about $63$ steps to fail. So we don't bounce from $A$ to $B$ to $A$ to $B$, etc.  
There is still one situation that we could address: when there are exactly two states on a palier, in this case, we **will** bounce back between the two. So we could keep the last visited state and prevent returning to it. I believe this will contribute to lowering the number of steps to fail. I am not entirely sure it will raise our success rate, though.

I won't cover it here, because that's a pretty trivial modification to make, but I encourage you to do it.

### Random Restart

Alright, this will be out last "improvement" on this algorithm. You will see in a minute why I put quotes.  
We will use a technique called "random restart" which is exactly what you expect it to be from its name. We let our algorithm run for 100 iterations (here we chose 100, but feel free to experiment) and if it fails to find a solution by then, it simply regenerates a new, random state and restarts from here.  
To be fair, when this happens we keep adding the number of moves it makes.

Here's how we can implement it:

```haskell
hillClimbingRandomRestart :: Bool -> IORef (Maybe Board) -> IO (Maybe (Bool, Int))
hillClimbingRandomRestart verbose board = do
    board' <- readIORef board
    case board' of
        Nothing     -> do
            when verbose $
                putStrLn "No board has been generated yet."
            return Nothing
        Just b      -> do
            let currentValue = countAttackingPairs b
            if (currentValue == 0) then do
                when verbose $ do
                    putStrLn "No need for algorithm, the board is already solved:"
                    displayBoard b
                return $ Just (True, 0)
            else do
                hillClimbingRandomRestart' 0 verbose board >>= return . Just

hillClimbingRandomRestart' :: Int -> Bool -> IORef (Maybe Board) -> IO (Bool, Int)
hillClimbingRandomRestart' stepNb verbose board = do
    Just b <- readIORef board
    let currentValue = countAttackingPairs b
    (succeeded, nbOfSteps) <- hillClimbingStep verbose 0 100 (b, currentValue) board
    when verbose $ do
        if succeeded then do
            putStrLn $ "Found a solution in " ++ show nbOfSteps ++ " steps."
        else do
            putStrLn $ "Solution not reached after " ++ show nbOfSteps ++ ", restarting on random state."
    if succeeded then
        return (True, stepNb + nbOfSteps)
    else do
        generateRandomN board 8
        hillClimbingRandomRestart' (stepNb + nbOfSteps) verbose board
```

Nothing changed much, the last two lines are the one who matter; when `hillClimbingStep` failed, rather than returning `False` and the number of steps, we just generate a new random board, and call back ourselves with a starting value of the `stepNb` parameter that corresponds to the number of steps `hillClimbingStep` took to fail.

Please note that this makes sense in our problem (the N-Queens problem) because as we saw earlier, the _result_ matters to us, not the way we arrive at it.  
Let me rephrase this just to mark the importance of what I've just said: what we want is to find a solution to the N-Queens problem, for a given $N$, we don't care about how our algorithm does it; while obviously prefering quick methods.

"Okay hot-shot, so I've just to find **one solution** and make my algorithm always return that solution, it will be $O(1)$". Well... yes. You could do that. That answers out goal "find a solution", but that would be pretty pointless, right? So we _did_ try to have a nice algorithm that can find several solutions when run several times.  
Anyway, I just wanted to highlight the fact that random-restart doesn't make sense in all situations, so don't think about it as a go-for-it solution every time.  
In our situation, though, it is pretty efficient: 

```zsh
Launching stats on 10000 computations.

###########################################################
Solved 10000/10000 (100.%).
It takes an average "22.4" steps to find a solution.
It takes an average "NaN" steps to fail.
###########################################################
```

Surprised?  
Well, hardly. From the code sample above, you can see that when an iteration failed, we don't return; instead we generate a new board and start all over again, until we succeeded. So _of course_ it will solve $100\%$ of the puzzles (provided it nevers encounters a case when it loops forever, which can't happen here because we limited the numer of lateral moves to $100$).

#### Just a Word Before You Become Mad

Okay you _might_ be thinking that it's cheating, and that is useless. Well, not entirely true.  
Your feeling might be that, since we allowed to randomly generate states as we like, we could just make our algorithm deadly simple: take a board, compute its value; if it is 0 then return the board as a solution, else, generate a new board at random and do it again.  
That is entirely true, this is called brute force. The good thing with brute force is that you are indeed guaranteed to find a solution (providing your evaluation function returns, but we'll suppose it is true). But unfortunately (or fortunately, depending on how you consider things) that's the only good aspect of brute force.  
Let me remind you that for our 8-Queens problem, there are about $17,000,000$ possibilities, and going through all of them to find a solution will take time. What **we** do, however is use a pretty good function (here, our `hill-climbing` algorithm, which results in a $94\%$ solving rate) to quickly find a solution. It does, most of the time. And on the rare occasions it doesn't ($6\%$), we indeed start off with a new state and do it again.

## How Does Our Algorithm Scale?

Everything we did until now was on $8\times8$ boards. But we know there are pretty costy steps involved here. So I modified the code we wrote together to ask us the dimension ofthe board before it runs tests (you can find the modifications [on the github repo](https://github.com/nschoe/common-ai-n-queens)).  
So remember the the results for $N=8$?

```zsh
Launching stats on 1000 computations for 8x8 boards.

###########################################################
Solved 936/1000 (93.6%).
It takes an average "18.5" steps to find a solution.
It takes an average "74.2" steps to fail.
###########################################################
```

Yeah, around $94\%$. Now let's try $10\times10$ boards:

```zsh
Launching stats on 1000 computations for 10x10 boards.

###########################################################
Solved 892/1000 (89.2%).
It takes an average "29.3" steps to find a solution.
It takes an average "93.8" steps to fail.
###########################################################
```

Ah, so we see that we drop a little. More importantly, it took way more time. And we only switched from $8\times8$ to $10\times10$.

Now I'm trying on $15\times15$:

```zsh
Launching stats on 1000 computations for 15x15 boards.

###########################################################
Solved 968/1000 (96.8%).
It takes an average "29.2" steps to find a solution.
It takes an average "86.2" steps to fail.
###########################################################
```

Okay... I have actually no idea why our success rate is _increasing_ now (I ran it twice, same thing). But it sure takes a lot more time.

I'm gonna try to run a $100\times100$...  
Okay not luck, it won't do it.  
I have actually tried to run **one** computation on a $100\times100$ board and it took so much time I had to stop it... for only one board.  
Actually I could see a message **every second** on the board saying `Looping on better state...`. It took that much time, only to generate all successors. This is where our algorithm fails short: it is definitely not scalable.

## Conclusion

I hope you liked it!  
As I said (or rather _wrote_) before, I'm still not sure what my public will be, so please, feel free to [tell me](mailto:ns.schoe@gmail.com) if there are too much explanations (I think that might be the case) or not enough (I doubt it, but still possible) or if they are not balanced enough (important points versus irrelevant ones).

### What We did

So we had a pretty good trip together on A.I. This might seem a bit lightweight for some of you, which is normal, after all, this is the first and the simplest algorithm of the four we will cover in this series of articles.  
We started by "defining" some terms and notions, in order to get all on the same grounds.  
Then we took an overview of the Hill-Climbing algorithm; we understood that the principle behind it was to compute the values of our states, choose the best one, and recurse.
Then we went for it: we developed the first, basic version of the Hill-Climbing algorithm, it achieved pretty poor results, but it _did_ achieve results nonetheless!  
We understood its limitations came from always selecting a strictly best state and we understood this constraints could be relaxed. So that is what we did: we relaxed the constraints to allow for lateral (_i.e._ not better nor worse) moves, but we took care of constraining the number of these lateral moves.  
Now we got something real: $94\%$ success rate is pretty awesome for a basic algorithm!  
And at last we saw the benefit of sometimes restarting from another point in the space of possible states, which can be useful.

As we saw, though, our algorithm performs badly when it deals with a board larger than, say, $15\times15$. It doesn't scale well, takes too much time and too much ressource.  
We will want to correct that in our next algorithms. 

### A Word On Random

I wanted to highlight quickly here the importance of the _random_ component. To improve our algorithm, we used random actions in two places:

- when we decided to take a lateral move, we saw that always taking the same (first element on the list) state increased our success rate to $30\%$ only, but when we introduced the random choice _among the states of same value_, it jumped up to $94\%$.
- when we decided to generate a new board when we reached the maximum number of moves.

It might seem counter-intuitive at first to use random movements when we are creating an Artificial _Intelligence_, but when you retrospect a little, you take hundreds on random decisions on a daily-basis. So yes, Artificial Intelligence _needs_ random, and we will confirm that more extensively in our next algorithms.

### What Now?

Well now... while you play around with it (make the code more efficient, maybe even make it multi-thread or something), I'll be writing this article's part II.  
The part II will be about another algorithm: _Simulated Annealing_, this is a technique borrowed from metallurgy, to cool down metals in such a way they acquire specific characteristics (become very strong but shatter in millions of small pieces when past its limits, rather than break into extremely-sharp blades).  
Okay you might be wondering what that has to do with A.I.; let's think about Simulated Annealing as a new, improved version of our Hill-Climbing algorithm, which would be between generating random states until we find a solution and never make a lateral move like we did in the first place.

More on this on part II, I hope to see you again soon!