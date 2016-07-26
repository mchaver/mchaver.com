---
title: Untyped lambda calculus
date: 2016-07-25
---

$\lambda x . x^2 + 1$ the function mapping $x$ to $x^2 + 1$.
$(\lambda x . x^2 + 1)(3)$

####Function construction principles

Abstraction: From an expression $M$ and a variable $x$ we can construct a new
expression: $\lambda x . M.$ This is the abstraction of x over M.

Application: From expressions $M$ and $N$ we can construct $M \; N$. This is
the application of $M$ to $N$.

evaluation of expressions.

β-reduction: formalization of the function evaluation process. Use substitution
formally expressed by means of square brackets $[$ and $]$: the expression
$M[x := N]$ represents M in which N has been substituted for x.
β-reduction of $(\lambda x . M)N$ to $M[x := N]$.

$(\lambda x . x^2 + 1)(3)$ reduces to $(x^2 + 1)[x := 3]$

application of M to N is only a first step in this procedure, it is the construction
of a new expressions, which , in a later state, might lead to the actual
execution of a function.

$\lambda(x,y).(x^2+y)$ is the same as $\lambda x . (\lambda y . (x^2 + y))$.

Expressions in lambda calculus are called $\lambda$-terms. The set of $\Lambda$
of all $\lambda$-terms is constructed.

1. Variable: If $u \in V$, then $u \in \Lambda$.
2. Application: If $M$ and $N \in \Lambda$, then $(M \; N) \in \Lambda$.
3. Abstraction: If $u \in V$ and $M \in \Lambda$, then $(\lambda u . M) \in \Lambda$.

abstract syntax:
$\Lambda = V | (\Lambda \Lambda) | (\lambda V . \Lambda)$

subterms of a $\lambda$-term
Multiset of subterms: Sub

1. Basis: $Sub(x) = \{x\}$, for each $x \in V$.
2. Application: $Sub((M \; N)) = Sub(M)\cup Sub(N) \cup \{(M \; N)\}$
3. Abstraction: $Sub((\lambda x . M)) = Sub(M) \cup \{(\lambda x . M)\}$.

####Variables

free:
bound
binding: occurrences immediately after a $\lambda$.

$FV$ the set of free variables of a $\lambda$-term.

1. Variable: $FV(x) = \{x\}$,
2. Application: $FV(M \; N) = FV(M) \cup FV(N)$,
3. Abstraction: $FV(\lambda x . M) = FV(M) \backslash \{x\}.$

Close $\lambda$-term $\Lambda^0$
The $\lambda$-term M is closed if $FV(M) = \emptyset$. A closed $\lambda$-term
is also called a combinator. The set of all closed terms is $\Lambda^0$.

####Alpha conversion
The name of the binding variable is not important in $\lambda$-calculus.

$\alpha$-conversion
$\alpha$-equivalence renaming binding and bound variables.

Renaming, $M^{x \to y}$, $=_ {\alpha}$
result of replacing every free occurrence of $x$ in $M$ by $y$.
$\lambda x . M =_ {\alpha} \lambda y . M^{x \to y}$ provided that
$y \notin FV(M)$ and $y$ is not a binding variable in $M$.

####Alpha equivalence

$M =_ {\alpha} N$ implies that the structures of $M$ and $N$ are the same except
for the names of the binding variables and the corresponding bound ones.

####Beta reduction

One-set β-reduction, $\to_ {\Beta}$

1. Basis: $(\lambda x . M)N \to_ {\beta} M [x := N]$
2. Compatibility: If $M \to_ {\Beta} N$, then $M \; L \to_ {\Beta} N \; L$,
$LM \to_ {\Beta} LN$ and $\lambda x . M \to_ {\Beta} \lambda x . N$.

redex: reducible expression.
contractum: right-hand side of the redex.

zero or more step beta reduction

$\Beta$-conversion
$M =_ {\Beta} N$ if there is $n \leq 0$ and there are terms $M_0$ to $M_n$ such
that $M_0 = M$, $M_n = N$ and for all such i that $0 \leq i < n$

either $M_i \to_{\Beta} M_{i+1}$ or $M_{i+1} \to_{\Beta} M_i$.

$\Beta$-normal form: outcome of a term (relation with reduction and conversion)

1. $M$ is in $\Beta$-normal form if $M$ does not contain any redex.
2. $M$ has a $\Beta$-normal form if there is an N in $\Beta$-nf such that
$M =_ {\Beta} N$. Such an $N$ is a $\Beta$-normal form of $M$.

####Fixed Point Theorem

Every $\lambda$-term $L$ has a fixed point, e.g. for each $L$ there exists a
$\lambda$-term $M$ such that $LM =_ {\Beta}M$.

Fixed point: borrowed from functional analysis, a function $f$ has a fixed point
$a$ if $f(a) = a$. Square of n has two, 0 and 1. Successor functions have no
fixed points.

Compatibility rules for reduction: how reduction extends from subexpressions to
bigger ones.
