---
title: Chapter 17 - Metatheory of Subtyping
date:  2016-07-17
---

Two new rules to handle issue with previous definition of subsumption (T-Sub)
and transitivity (S-Trans).

Algorithmic Subtyping
Algorithmic Typing

$S <: T$ is derivable from algorithmic subtyping rules iff it is derivable from
declarative rules. A term is typable by algorithmic typing rules iff it is
typable under declarative rules.

###Algorithmic Subtyping
Subtype checker is called by the type checker. Example, application $t_1 t_2$
where $t_1$ has types $T \to U$ and $t_2$ has type $S$. Subtype checker must decide
whether $S <: T$ is derivable from the subtyping rules. It checks whether $(S,T)$
belongs to another relationship $\mapsto S <: T$ "S is algorithmically a subtype
of T". Membership can be decided simply by following the structure of the types.

Use transitivity to paste together subtyping derivations for records involving
depth, width and permutation subtyping.

Syntax for Algorithmic Subtyping

SA-Top $\mapsto S <: Top$

S-RCD
\\[
\\frac{{l_i^{i \\in 1..n}} \\subset {k_j^{j \\in 1..m}} \\qquad k_j = l_i \\; implies \\; S_j <: T_i}
      {{k_j : S_j ^{j\\in 1..m}} <: {l_i : T_i^{i \\in 1..n}}}
\\]

SA-Arrow
\\[
\\frac{\\mapsto T_1 <: S_1 \\qquad \\mapsto S_2 <: T_2}
      {\\mapsto S_1 \\to S_2 <: T_1 \\to T_2}
\\]

Proposition Soundness and Completeness: $S <: T iff \mapsto S <: T$

Algorithmic Subtyping Pseudocode
```
subtype(S,T) =
  if T = Top, then true
  else if S = S -> S1 and T = T1 -> T2
    then subtype (T1,S1) && subtype (S2, T2)
  else if S = {k_j : S_j^{j \in 1..m}} and T = {l_i : T_i^{i \in 1..n}}
    then {l_i^{i\in 1 ..n}} \subset {k_j^{j \in 1..m}}
      \wedge for all i there is some j \in 1..m with k_j = l_i
        and subtype(S_j, T_i)
  else false.
```

Proposition Termination: If $\mapsto S <: T$ is derivable, then $subtype(S,T)$
will return true. If not, then $subtype(S,T)$ will return false. function is total
the algorithmic rules will return either true or false in a finite amount of time.
subtype(S,T) must always return something, it cannot diverge.

Algorithmic Typing Rules

(fill in)

The algorithmic typing relations is the least relation closed under their rules.

###Joins and Meets
Typechecking expressions with multiple result branches (conditional, case) requires
that all branches return the same type.

join: A type J is called a $join$ of a pari of types S and T, written $S \vee T = J$
if $S <: J, T <: J$ and for all types U, if $S <: U$ and $T <: U$, then $J <: U$.

meet: A type M is a $meet$ of S and T, $S \wedge T = M$ if $M <: S$,$M<:T$ and for
all types L, if $L <: S$ and $L <: T$, then $L <: M$.

A subtype relations has joins if for every S and T there is some J that is a
join of S and T.

A subtype relation has meest if for every S and T, there is some M that is a
meet of S and T.

${}$ and $Top \to Top$ do not have any common subtypes at all. However, a pair
of types S and T is said to be "bounded below" if there is some type L such that
$L <: S$ and $L <: T$.

"bounded meets" for every S and T such that S and T are bounded below, there is
some M that is a meet of S and T.

Proposition Existence of Joins and Bounded Meets

1. For every pair of types S and T, there is some type J such that $S \vee T = J$.
2. For every pair of types S and T with a common subtype, there is some type M
such that $S \wedge T = m$.

###Algorithmic Typing and the Bottom Type

Need to extend the subtype relation, the subtyping and typing algorithms must
be extended.

SA-Bot $\mapsto Bot <: T$

TA-AppBot

\\[
\\frac{\\Gamma \\mapsto t_1 : T_1 \\quad T_1 = Bot \\qquad \\Gamma \\mapsto t_2 : T_2}
      {\\Gamma \\mapsto t_1 t_2 : Bot}
\\]

TA-ProjBot

\\[
\\frac{\\Gamma \\mapsto t_1 : R_1 \\quad R_1 = Bot}
      {\\Gamma \\mapsto t_1 . l_i : Bot}
\\]

We can apply something of type Bot to an argument of absolutely any type and assume
that the result has any other type, same for projection.
