---
title: Type Reconstruction
date:  2016-07-17
---

Calculate a principal type for a term in which some or all of these annotations
are left unspecified.

###Type Variables and Substitutions

Treat uninterpreted base types as type variables which can be substituted or
instantiated.

describing a mapping $\sigma$ from type variables to types (type substitution)
apply mapping to a particular type T to obtain an instance $\sigma T$.

example, $\sigma = [X \mapsto Bool]$, apply $\sigma$ to $X \to X$ to obtain
$\sigma(X \to X) = Bool \to Bool$

Type Substitution: finite mapping from type varaibles to types. $[X \mapsto T, Y \mapsto U]$
for the substitution that associates X with T and Y with U.
$dom(\sigma)$ for the set of type varaibles appearing on the left-hand sides
of pairs in $\sigma$. $range(\sigma)$ for the set of types appearning on the
right-hand sides.

\\[
\\sigma(X) = T if (X \\mapsto T) \\in \\sigma
             X if X is not in the domain of \\sigma
\\sigma(Nat)  = Nat
\\sigma(Bool) = Bool
\\sigma(T_1 \\to T_2) = \\sigma T_1 \\to \\sigma T_2
\\]

substitution composition: $\signma \cdot \gamma$

\\[
\\sigma \\cdot \\gamma = X \\mapsto \\sigma (T)for each (X \\mapsto T) \\in \\gamma
                         Y \\mapsto T for each (X \\mapsto T) \\in \\sigma with X \\notin dom(\\gamma)
\\]

Theorem Preservation of Typing under Type Substitution: If $\sigma$ is any type
substitution and $\Gamma \vdash t : T$, then $\sigma \Gamma \vdash \sigma t : \sigma T$

###Two Views of Type Variables

t is a term containing type variables and $\Gamma$ is an associated context.

1. Are all substitution instances of t well typed? For every $\sigma$, is there
$\sigma \Gamma \vdash \sigma t : T$ for some T?
2. Is some substitution instance of t well typed? Can we find a $\sigma$ such that
$\sigma \Gamma \vdash \sigma t : T$ for some T

parametric polymorphism for the first one.

t may not be well typed, can it be instantiated to a well typed term by choosing
appropriate values for some of its type variables. Looking for valid type variables
is type reconstruction (type inference).

Type reconstruction

Let $\Gamma$ be a context and $t$ a term. A solution for $(\Gamma , t)$ is a pair
$(\sigma , T)$ such that $\sigma \Gamma \vdash \sigma t : T$.

###Constraint-Based Typing

A constraint set C is a set of equations ${S_i = T_i^{i \in 1..n}}$. A substitution
$\sigma$ is said to unify an equation $S = T$ if the substitution instances $\sigma S$
and $\sigma T $ are identical. \sigma unifies (satisfies) C if it unifies every
equation in C.

$\Gamma \vdash t : T | x C$ term t has type T under assumptions $\Gamma$ whenver constraints C are satisfied.
X subscripts are used to track the type variables introduced in each subderivation
and make sure that the fresh variables created in different subderivations are
actually distinct.

Suppose that $\Gamma \vdash t : S | C$. A solution for $(\Gamma,t,S,C)$ is a
pair $(\sigma, T)$ such that $\sigma$ satisfies C and $\sigma S = T$.

Two ways of characterizing ways of instantiating type variables:

1. Declarative: set of all solutions for $(\Gamma, t)$
2. Algorithmic: via the constraint typing relation, by finding S and C such that
$\Gamma \vdash t : S | C$ and then taking the set of solutions for $(\Gamma,t,S,C)$.

Theorem Soundness of Constraint TYping: Suppose that $\Gamma \vdash t : S | C$.
If $(\sigma, T)$ is a solution for $(\Gamma,t,S,C)$, then it is also a solution
for $(\Gamma, t)$.

$\sigma \\ X$ for the substitution that is undefined for all the variables in X
and otherwise behaves like $\sigma$.

Theorem Completeness of Constraint Typing: Suppose $\Gamma \vdash t : S | x C$.
If $(\sigma,T)$ is a solution for $(\Gamma, T)$ and $dom(\sigma) \cup X = \emptyset$
then there is some solution $(\sigma ',T)$ for $(\Gamma, t, S, C)$ such that
$\sigma ' \\ X = \sigma$.

###Unification
