---
title: The encoding of logical notions in Calculus of Constructions
date: 2016-08-04
---

####Absurdity and negation in type theory

$A \to B$ as logical implication. Need to encode negation, conjunction and
disjunction to make a more robust logic system. It is not possible in $\lambda P$
but it is in $\lambda C$.

The negation $\neg A$ as implication $A \implies \bot$ where $\bot$ is absurdity
or contradiction.

Absurdity property
If $\bot$ is true, then every proposition is true.
"Ex falso sequitur quodlibet" from an absurdity follows whatever you like.
If $\bot$ is inhabited, then all propositions A are inhabited.
If we have an inhabitant $M$ of $\bot$, then there exists a function mapping an
arbitrary proposition $\alpha$ to an inhabitant of this same $\alpha$.
This function has a type $\Pi \alpha : \ast . \alpha$

If there is such an $f$, then we can make all propositions true.

By defining $\bot$ ad $\Pi \alpha : \ast . \alpha$, we get$\bot$-elimination
for free.

####Negation

$\bot$-introduction

####Conjunction

Second order encoding of conjunction
$A \wedge B$ is $\Pi C : \ast . (A \to B \to C) \to C$
read $\Pi$ as forall and $\to$ as implies.
For all C, (A implies (B implies C)) implies C
If A and B together imply C, then C holds on its own.

####Disjunction

$A \vee B$ is $\Pi C : \ast . (A \to C) \to (B \to C) \to C$.
