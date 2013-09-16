---
title: An introduction to dynamic programming
author: Mog
---

Last year, for the french national event ''les Journées de la
Science'', two friends and I explained to high-schoolers the basics of
dynamic programming. Here I'll try to retranscript what we did.

Complexity
==========

Intuition
---------

First, we gave an intuitive explanation of complexity: let's say you
are a teacher and are about to enter your classroom. The first thing
you'll do when you'll be inside it will be to say hi to your students.

There are several different ways you could do that. Amongst them, you could:

- wave your hand at all your students at once
- shake the hands of your students one by one

We can see that the first way of saying hi will always take one action
no matter how many students you have. However, the second way will
take as many actions as you have students. We'll say the first method
has a constant complexity regarding the number of students while the
second has a complexity that grows with the number of students.

Why it matters
--------------

Then, we went on with a little story that is [very well known][1]:

> Once upon a time, a guy came up with the principles of the game of
> chess. He showed his game to his king and his king was really
> pleased with it. He said the our game maker:
> 
> --- For such a great game, I'll give you whatever you want. Just ask!
> 
> Our inventor was a very clever man, he replied:
> 
> --- I am very humble. I'd just like you to put one grain of wheat on
>     the first cell of my chessboard, two on the second, four on the
>     third, eight on the fourth and so on.
> 
> Surprised by the seemingly small reward his subject asked for, the
> king agreed.
> 
> Not long after that, the mathematicians of the king warned him that
> if he were to keep his word, the country would be ruined because
> the number of wheat grains necessary was well beyond the current
> nation stocks. Very suprised, the king asked for an explanation.
> Here is what the mathematicians showed to him:
> 
> --- by the 10th cell, 512 grains were required
> 
> --- by the 20th cell, 524 288 grains were required
> 
> --- by the last cell, 9 223 372 036 854 775 808 grains were required
> 
> Then pick your ending, either the king was very displeased and
> killed our poor bright guy or he found the joke quite funny and
> made him his minister ;)

What we see here is the effect of a very bad kind of complexity: an
exponential one. We say an action has exponential complexity regarding
some quantity $n$ when the number of steps required to complete the
action grows as something to the $n$. Here for example the number of
grains required grows exponentially with the number of cells
considered. We can say that $g_i=2^{i}$ where $g_i$ is the number of
grains of wheat on the cell $i$ (starting from 0).

Dynamic programming
===================

After this introduction to complexity, we went on and tackled an
[Euler problem][2] about [Manhattan distance][3] that has an
exponential complexity if treated naively.

The idea is to find out how many paths there are in a grid between the
top left corner and the bottom right corner if you are only allowed to
go bottom or right.

The naive algorithm will follow every path possible. Spoilers, for the
20x20 grid mentionned in the original problem, that is about 140
billion paths.

Here we introduce dynamic programming or the art of finding a
recursion that allows us to circumvent the complexity explosion.

We can come to any given cell from only two cells. The one above it
and the one to its left. Now, if we were able to know how many paths
led to those two cells, we could say that the sum of those two numbers
would be the number of path leading to our cell:

$$P_{cell} = P_{above} + P_{left}$$

And if we start methodically from the top left corner, this
requirement about the parent cells is easily met: we start with one
path going from the corner to the cell to its right and one path going
from the corner to the cell that is below it.

Then it's just a matter of completing the intermediate path counts
until we are able to calculate the path counts of the bottom right
corner.

To summarize, we've circumvented the exponential complexity by solving
only a really small part of the problem (one step) and by applying a
recursive method to gradually get to the solution. That's what dynamic
programming is about. Obviously, finding the recursion gets
complicated some times, but the basics are simple!

[1]: https://en.wikipedia.org/wiki/Wheat_and_chessboard_problem
[2]: https://projecteuler.net/problem=15
[3]: https://en.wiktionary.org/wiki/Manhattan_distance