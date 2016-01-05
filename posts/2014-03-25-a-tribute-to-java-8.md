---
title: A tribute to Java 8
author: m09
---

Java 8 got released a few days ago and I am very excited about
it. Basically it incorporates some very nice features of Scala and
Haskell and it overall improves the experience of using the language
by a lot.

I've decided to come back to Project Euler recently and thought I
might try Java 8 for that. I quickly rolled a basic application to
manage the problems launch and got to work on them. A particularly
interesting one is the [19<sup>th</sup> problem][problem]. Especially
if we compare the Haskell solution I previously had to the Java one I
came up with.

<div></div><!--more-->

The problem is about finding the months which started on a sunday
during the 20<sup>th</sup> century.

This is quite straightforward and here is the main function I used to
solve it in Haskell (nDays returns the number of days in a month. The
rest of the file is [on github][githaskell]):

``` haskell
main = print
     . length
     . filter (== 6)
     . drop 12
     . scanl (\o m -> (o + nDays m) `mod` 7) 0
     . takeWhile (< QMonth (2001, 1))
     . iterate nextMonth
     $ QMonth (1900, 1)
```

Basically, I iterate over an infinite list (a stream) of successive
months, I do some computations with the number of days in each month
in order to determine if each month was starting on a sunday or not
and that's it.

The `scanl` syntax is a bit obscure at first but the rest should be
understandable, when you know that the value 6 represents a sunday
because the 1<sup>st</sup> of January 1900 was a monday, so the offset
corresponding to sundays is 6.

The very nice thing I found out is that since Java 8 came out, there's
no need to adapt this algorithm (well, almost no need). We can also go
through an infinite list of months and perform operations on the fly
(the rest of this file is also [on github][gitjava]):

``` java
public String getSolution() {
    return Stream.iterate(new MonthDate(1, 1901), this::nextMonth)
            .limit(100 * 12)
            .map(m -> nDays(m))
            .reduce(
                    Pair.of(0, 2),
                    (u, n) -> {
                        return Pair.of(
                                u.getLeft() + (u.getRight() == 0 ? 1 : 0),
                                (u.getRight() + n) % 7);
                    },
                    (u, v) -> Pair.of(u.getKey() + v.getKey(),
                            (u.getRight() + v.getRight()) % 7))
            .getKey()
            .toString();
}
```
The API isn't perfect yet (no `takeWhile` really sucks when dealing
with streams). But we can already see that we can set up nice
pipelines in a breeze.

The very nice thing about those pipelines is that they are
parallelizable without any effort, just like in Haskell!

I'm very much looking forward to see how Java evolves in the future.
Lambdas, streams, function pointers and so on sure seem to be going in
the right direction.

[problem]: https://projecteuler.net/problem=19
[githaskell]: https://github.com/m09/project-euler/blob/master/haskell/11-20/19.hs
[gitjava]: https://github.com/m09/project-euler/blob/master/java/src/main/java/eu/crydee/projecteuler/problems/P19.java
