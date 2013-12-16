---
title: A Hidden Markov Model attempting to write
author: Mog
---

<style>
img.post {
    display: block;
    margin: 20px auto 20px auto;
}
</style>

Recently I trained Hidden Markov Models to recognize digits in an
automated fashion and came across a beautiful moment: a model writing
digits in a way children would. Let me show that to you.

<div></div><!--more-->

A Hidden Markov Model is an interesting yet simple beast. It is an
automaton that uses a hidden process to modelize some
observation. Here for example, we use the stage of the digit
writing---beginning, middle or end--- to model the probability of
observing a certain angle between to consecutive points.

Consider the following 8 different types of angles:

<img class="post" src="/posts/2013-11-16-a-hidden-markov-model-attempting-to-write/directions.svg">

And the following digit:

<img class="post" src="/posts/2013-11-16-a-hidden-markov-model-attempting-to-write/1.svg">

Here our 3 states HMM would tell us that 1 will most likely produce 2s
and maybe some 7s during its initial drawing stage, then that it will
produce 7s during its middle and end stages.

Now the interesting part: after having trained such an HMM, but with
10 different stages instead of 3, we can see which direction the HMM
produces most likely at each step. And that allows to represent what
the HMM thinks a digit is in the most probable case:

<img class="post" src="/posts/2013-11-16-a-hidden-markov-model-attempting-to-write/writings.svg" width="400">

What a gifted automaton!
