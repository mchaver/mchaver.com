---
title: Chapter 22 - Type Reconstruction
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

Hindley (1969)
Milner (1978)
Robinson (1971)

Check that the set of solutions is nonempty and find a best element, in the sense
that all solutions can be generated straightforwardly from this one.


###Principal Types

If there is some way to instantiate the type variables in a term so that it
becomes typable, then there is a most general or principal way of doing so .

A principal solution for $(\Gamma,t,S,C)$ is a solution $(\sigma, T)$ such that,
whenever $(\sigma',T')$ is also a solution for $(\Gamma,t,S,C)$, we have
$\sigma \sqsubseteq \sigma'$. When $(\sigma, T)$ is a principal solution, we
call T a principal type of t under $\Gamma$.

Theorem Principal Types: If $(\Gamma,t,S,C)$ has any solution, then it has a
principal one. The unification algorithm can be used to determine if $(\Gamma,t,S,C)$
has a solution and calculate a principal one.

The idea of principal types can be used to build a type reconstruction algorithm
that works more incrementally. Instead of generating all constraints first and
then solving it, we can interleave generation and solving, so that the type
reconstruction algorithm returns a principal type at each step. These reinsures
that the algorithm never needs to re-analyze a subterm, makes minimum commitments
needed to achieve typability at each step. It helps pinpoint errros in the
user's program much more precisely.

###Implicit Type Annotations

Languages supporting type reconstruction typically allow programmers to
completely omit type annotations on lambda-abstractions. One way to achieve this
is to make the parser fill in omitted annotations with freshly generated type
variables. A better alternative is to add un-annotated abstractions to the
syntax of terms and a corresponding rule to the constrain typing relation.

CT-AbsInf (make several copies of un-annotated abstractions, allow us to choose
  different variable as the argument type of each copy).

\\[
\\frac{X \\notin \\mathbb{X} \\quad \\Gamma, x : X \\vdash t_1 : T | \\mathbb{X} C}
      {\\Gamma \\vdash \\lambda x . t_1 : X \\to T | \\mathbb{X} \\cup {\\x} C }
\\]

###Let-Polymorphism

Associate a different variable X with each use of a function. Alter the ordinary
typing rule for let so that it substitutes $t_1$ for x in the body and then
typechecks the expanded expression.

T-Let

\\[
\\frac{\\Gamma \\vdash t_1 : T \\qquad \\Gamma, x : T_1 \\vdash t_2 : T_2}
      {\\Gamma \\vdash let \\; x = t_1 \\; in \\; t_2 : T_2}
\\]

T-LetPoly

\\[
\\frac{\\Gamma \\vdash [x \\mapsto t_1]t_2 : T_2 \\qquad \\Gamma \\vdash t_2 : T_1}
      {\\Gamma \\vdash let \\; x = t_1 \\; in \\; t_2 : T_2}
\\]

CT-LetPoly

\\[
\\frac{\\Gamma \\vdash [x \\mapsto t_1]t_2 : T_2 \\qquad \\Gamma \\vdash t_2 : T_1 | \\mathbb{X} C}
      {\\Gamma \\vdash let \\; x = t_1 \\; in \\; t_2 : T_2 | \\mathbb{X} C}
\\]

E-LetV

\\[ let \\; x = v_1 \\; in \\; t_2 \\to [x \\mapsto v_1] t_2  \\]

perform a step of evaluation before calculating the types.

special type checking algorithm for embedded lets.
