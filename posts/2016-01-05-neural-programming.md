---
title: Neural Programming
author: m09
---

I gave a [talk][slides] introducing three interesting papers on Neural
Programming yesterday for a reading group meeting of the
[Nantes Machine Learning Meetup][nmlm]. Let me tell you about my
favorite!

<div></div><!--more-->

The goal of Neural Programming is to learn programs automatically by
using Neural Networks. It's a problem that's both interesting and
important. Indeed, that combination makes it a great target for
researchers.

The deadline for ICLR'2016 submissions was around early November of
2015 and I noticed many papers focusing on this task. In this meetup
we looked into three of them:

- [Neural Programmer: Inducing Latent Programs with Gradient Descent][np]
- [Neural Programmer-Interpreters][npi]
- [Neural Random-Access Machines][nram]

The paper I found most interesting is the second one, Neural
Programmer-Interpreters. It introduces or uses many interesting
concepts:

- program embeddings, stocked on an external memory so that they can
  be re-used.
- stack execution, to simulate the classical C style execution of
  functions. This is an elegant way to enable arbitrary compositions
  of programs.
- curriculum learning: first learn simple programs so that complex
  programs can be learned by composing them instead of learning
  everything from scratch.
- continual learning: once you have learned many programs, no need to
  redo everything from scratch to add some programs. Just freeze what
  you've learned and learn the new program embeddings.

I highly encourage you to read the paper, it's worth it!

[nmlm]: http://www.meetup.com/Nantes-Machine-Learning-Meetup/
[np]: http://arxiv.org/abs/1511.04834
[npi]: http://arxiv.org/abs/1511.06279
[nram]: http://arxiv.org/abs/1511.06392
[slides]: https://rawgit.com/m09/NMLM/master/2016-01-04/index.html
