---
title: Types dependent on terms
date: 2016-07-28
---

A type depending on a term has the general format: $\lambda x : A . M$. This
abstraction depends on the term $x$. A type depending on a term is a
typed-value function, $\lambda P$.

Propositions as types (PAT-interpretation).

$\Pi$-type now is a dependent product in which the output type depends on the
value of x chosen in the input type. Form (product rule) enables the construction
and typing for $\Pi$-type (the Cartesian product of a family of types).

\\[
\\frac{\\Gamma \\vdash A : \\ast \\quad \\Gamma, x : A \\vdash B : s}
      {\\Gamma \\vdash \\Pi x : A . B : s}
\\]

Many rules in $\lambda P$ have a double role since $s$ (metavariable) may be
either $\ast$ or $\square$.

Typing questions for $\lambda P$:

1. Well-typedness: if $\lambda x : A . \lambda y : P x . y$ is well typed.
2. Type Checking: check that $A : \ast, P : A \to \ast \vdash \lambda x : A . \lambda y : P x . y : \Pi x : A . P x \to P x$
3. Term Finding: find a term of type $\Pi x : A. P x \to P x$ in the context
$A : \ast, P : A \to \ast$.


####Minimal predicate logic

It is possible to code a simple form of logic in $\lambda P$ which has implication
and universal quantification. The basic entities are propositions, sets and
predicates over sets.

PAT:

1. If a term b inhabits type B ($b: B$), where B is interpreted as a proposition ,
then we interpret b as a proof of B. Such a term b in type theory is called a
proof object.
2. On the other hand, when no inhabitant of the proposition b exists, then there
is no proof of B, so B must be false.

####Sets
Set S as a type, $S: \ast$. Elements of sets are terms. a is an element of set S,
then $a : S$

####Propositions
If A is a proposition, then $A : \ast$. A term p inhabiting such A codes a proof
of A. A is a true proposition, p is a proof of A, then $p : A$.

####Predicates
A predicate P is a function from a set S to the set of all propositions, $P : S \to \ast$.
If P is an arbitrary predicate on S, then for each $a : S$ we have that $P a : \ast$.
$P \; a$ are propositions, which are types (level 2), each $P \; a$ may be inhabited:

####Implication
A implies B can be seen as a function $A \to B$.

####Universal Quantification
$\forall_{x \in S} (P (x))$ of some predicate P depending on x, over a set S.
In type theory it is $\Pi x : S . P x$
