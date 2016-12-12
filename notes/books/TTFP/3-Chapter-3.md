---
title: Second order typed lambda calculus (System F)
date: 2016-07-27
---
In Simple typed lambda calculus one can construct terms depending on terms.

Abstraction: term M, in which x may occur as a free variable. Assume
x has the type $\sigma$, may abstrac M from x by means of $\lambda$, in order
to obtain $\lambda x : \sigma . M$. Every free occurrence of x in M becomes
bound in $\lambda x : \sigma . M$.

The term $\lambda x : \sigma . M$ depends on the term $x$. We can construct
terms depending on terms.

Application: the counterpart of abstraction. Apply a term from a term. $M N$ apply
the term M to the term N.

First order: terms depending on terms.

Second order: terms depending on types.

Add another abstraction to the front of a lambda:
$\lambda \alpha : * . \lambda x : \alpha . x.$

$\ast$ denotes the type of all types, $\alpha : \ast$.

Second iteration: $D_{\sigma, F}$ or $F \circ F$. $\lambda x : \sigma . F(F \; x)$.
$D$ for an arbitrary $\sigma$ and arbitrary $F : \sigma \to \sigma$:
$D$ as $\lambda \alpha : I . \lambda f : \alpha \to \alpha . \lambda x : \alpha . f  (f \; x)$.

$D$ is a polymorphic function because it generalizes over types.

####$\Pi$-types

$\Pi$-type is a type-binder $\Pi \alpha : * . \alpha \to \alpha$ for the type
of functions sending an arbitrary type $\alpha$ to a term of type $\alpha \to \alpha$.

$\lambda \alpha : * . \lambda x : \alpha . x : \Pi \alpha : * . \alpha \to \alpha$.

####Application Rules

($abst_2$) Second order abstraction rule
\\[
\\frac{\\Gamma, \\alpha : * \\vdash M : A}
      {\\Gamma \\vdash \\lambda \\alpha : * . M : \\Pi \\alpha : * . A}
\\]

When M has type A in a context where $\alpha : \ast$, then $\lambda \alpha : * . M$
has type $\Pi \alpha : * . A$.

($appl_2$) Second order application rule
\\[
\\frac{\\Gamma \\vdash M : \\Pi \\alpha : * . A \\qquad \\Gamma \\vdash B : * }  
      {\\Gamma \\vdash M B : A [\\alpha := B ]}
\\]

####The system $\lambda 2$

Abstract syntax: $\mathbb{T}2 = \mathbb{V} | (\mathbb{T} 2 \to \mathbb{T} 2) | (\Pi \mathbb{V}: * . \mathbb{T} 2)$

Pre-typed $\lambda 2$-terms:

$\Lambda_{\mathbb{T}2} = V \; | \; (\Lambda_{\mathbb{T}2} \Lambda_{\mathbb{T}2}) \; | \; (\Lambda_{\mathbb{T}2} \mathbb{T}2) \; | \; (\lambda V : \mathbb{T}2 . \Lambda_{\mathbb{T}2}) \; | \; (\lambda \mathbb{V} : * . \Lambda_{\mathbb{T}2})$.

Context is stricter in $\lambda 2$, treat the type variables on a par with term
variables. All variables must be declared before they may be used.

1. Empty set: $\emptyset$
2. If $\Gamma$ is a context, $\alpha \in \mathbb{V}$ and $\alpha \notin dom(\Gamma)$,
then $\Gamma, \alpha : \ast$ is a context; $dom(\Gamma, \alpha : \ast ) = (dom(\Gamma), \alpha)$.
3. If $\Gamma$ is a context, if $\rho \in \mathbb{T}2$ such that $\alpha \in dom(\Gamma)$
for all free type variables $\alpha$ occurring in $\rho$ and if $x \notin dom(\Gamma)$,
then $\Gamma, x : \rho$ is a context; $dom(\Gamma,x : \rho) = (dom(\Gamma), x)$

Legal: A term M is legal is there is a context $\Gamma$ and a type $\rho$ in
$\mathbb{T}2$ such that $\Gamma \vdash M : \rho$.
