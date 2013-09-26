---
title: A nice maths problem: the B52 and the submarine
author: Mog
---

Problem statement
=================

A B52 tries to bomb a submarine. It can't see it and has no radar or
anything of the sort. It can locate itself on a ℕ×ℕ grid though.

What we know about the submarine is the following: it starts on whole
coordinates and at each step, it moves by a vector whose dimensions
are whole too. For example, for a submarine that starts on `(10, -5)`
and has a move vector of `(-6, 1)`, we would have the following first
6 steps:

                        |
                        |
                        |
                        |
                        |
    5-------------------+-------------------------
          4             |
                3       |
                      2 |
                        |   1
                        |         0
The goal is to come up with a strategy to eventually bomb the
submarine.

Idea
====

The idea here is to somehow enumerate all the possible submarines. If
we can enumerate them, we can just try to bomb a possible submarine at
a given step.

For example, at step `i`, if we're trying to bomb the possible
submarine that starts at `(10892, -654)` and moves by a vector of
`(87, 73)` at each step, we'll bomb the cell `(10892 + 87 * i, -654 +
73 * i)`

Enumeration
===========

First, we can remark that a submarine is defined by a four-dimensions
vector `(x, y, dx, dy)` in ℕ<sup>4</sup>:

- x of the starting point
- y of the starting point
- dx of the movement at each step
- dy of the movement at each step

If you already proved that ℕ<sup>n</sup> is in bijection with ℕ, you
can stop there, you've just proved that there is an enumeration of all
possible submarines. Otherwise, let's get started.

The idea of the proof is that we can code any dimension of a
n-dimensions vector as a power of a prime. If we want to handle
positive and negative numbers, we could for example use the even
numbers for positive numbers and off numbers for the negative
ones. Then, if we take the product of all the primes we used
(therefore coding all the dimensions), we'll obtain a unique number,
through unique prime decomposition.

For example, we'd code `(156, -7, 89, 2)` as 2<sup>312</sup> ×
3<sup>13</sup> × 5<sup>178</sup> × 7<sup>4</sup>.

There it is, we can enumerate all possible submarines and we can bomb
them one after the other. At some point we'll bomb the real one!