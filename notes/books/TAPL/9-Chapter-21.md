---
title: Chapter 18 - Metatheory of Recursive Types
date:  2016-07-17
---

Theoretical foundations of typecheckers for equi-recursive types. Use coinduction
to make intuition precise.

Assume some universal set U. U represents the set of everything in the world, and
inductive or coinductive definition will be to pick out some subset of U. F is
referred to as a generating function.

$F \in P(U) \to P(U)$ is monotone if $X \subeteq Y$ implies $F(X) \subseteq F(Y)$

Let X be a subset of U

1. X is F-closed if $F(X) \subseteq X$. (run F on X, get same thing back)
2. X is F-consistent if $X \subseteq F(X)$. (run F on X for x (subset of X), everything in F(x) will have x)
3. X is a fixed point of F if $F(X) = F$.

U as some statements or assertions
F as a justification relation that given some set of statements (premises), tells
us what new statements (conclusions) follow from them.

F-closed set: cannot be made any bigger by adding elements justified by F, it
already contains all the conclusions that are justified by its member.

F-consistent is one that is self-justifying, every assertion in it is
justified by other assertions that are also in it

fixed ppoint of F is a set that is both closed and consistent, includes all the
justifications required by its members, all the conclusions that follow from its members,
nothing else.

Theorem Knaster-Tarski

1. The intersection of all F-closed sets is the least fixed point of F. $\mu F$
2. The union of all F-consistent sets is the greatest fixed point of F. $vF$

Principle of Induction: If X is F-closed, then $\mu F \subseteq X$.
Principle of Coinduction: If X is F-consistent, then $X \subseteq vF$.

###Finite and Infinite Types
How to view types as finite or infinite tress.
Three type constructors: $\to$,$\times$,$Top$.
${1,2}\star$ for set set of sequences of 1s and 2s
\cdot as empty sequence
$i^k$ k copies of i
$\pi$ $\sigma$ are sequences
$\pi,\sigma$ denotes concatenation of $\pi$ and $\sigma$.

A tree type is a partial function $T \in {1,2}\star \to {\to, \times, Top}$
 - $T(\cdot)$ is defined
 - if $T(\pi,\sigma)$ is defined then $T(\pi)$ is defined
 - if $T(\pi) = \to$  or $T(\pi) = \times$ then $T(\pi,1)$ and $T(\pi,2)$ are defined
 - if $T(\pi) = Top$ then $T(\pi,1)$ and $T(\pi,2)$ are undefined. 
