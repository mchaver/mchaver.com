---
title: Chapter 15 - Subtyping
date:  2016-07-16
---

Subtyping or Subtype Polymorphism. Interacts with most other language features
in non-trivial ways. Generally found in object-oriented languages.

###Subsumption

Without subtyping, the rules of the simply typed lambda-calculus are rigid.
Argument types exactly match the domain types of functions leads to a typechecker
that rejects many programs, that, to the programmer, seem obviously well-behaved.

Goal of subtyping: refine typing rules so they can accept types that are more
informative than necessary.

$S <: T$ S is a subtype of T, any term of S can safely be used in a context
where term of type T is expected. Principle of Safe Substitution.
"every value described by S is also described by T", or "the elements of S are
a subste of the elements of T".

T-Sub: if $S <: T$, then every element $t# of S is also an element of T.

\\[
\\frac{\\Gamma \\vdash t : S \\qquad S <: T}
      {\\Gamma \\vdash t : T}
\\]

###The Subtype Relation

Subtyping should be reflexive.
S-Refl $S <: S$

Subtyping should be transitive.
S-Trans
\\[
\\frac{S <: U \\qquad U <: T}
      {S <: T}
\\]

S-RcdWdith (Width Subtyping rule)
\\[{l_i : T_i^{i \\in 1..n+k}} <: {l_i : T_i^{i \\in 1..n}}\\]
Smaller subtype has more fields. ${x:Nat}$ set of all records with at least a
field x of type Nat. The set of values bleoning to the second type is a proper
subset of the set belonging to the first type. Width Subtyping rule only applies
to record types where the common fields are identical.

S-RcdDepth (Depth Subtyping)

\\[
\\frac{\\forall i \\qquad S_i <: T_i}
      {{l_i : S_i^{i \\in 1..n}} <: {l_i : T_i^{i \\in 1 ..n}}}
\\]

S-RcdPerm (Record Permutation) order of records does not matter in subtyping

\\[
\\frac{{k_j : S_j^{j \\in 1..n}} is \\; a \\; permutation \\; of \\; {l_i : T_i^{i \\in 1..n}}}
      {{k_j : S_j^{j \\in 1..n}} <: {l_i : T_i^{i \\in 1..n}}}
\\]

S-Arrow specify under what circumstances it is safe to use a function of one type
in a context where a different function type is expected.

\\[
\\frac{T_1 <: S_1 \\qquad S_2 <: T_2}
      {S_1 \\to S_2 <: T_1 \\to T_2}
\\]

Left-hand side is reversed (contravariant), right hand side is covariant. If we
have a function of type $S_1 \to S_2$ then we know that f accepts elements of
type $S_1$, f will also accept elements of any subtype $T_1$ of $S_1$. The type
of f returns elemnts of type $S_2$, any supertype $T_2$ of $S_2$.

S-Top (supertype for every type) Top is a maximum element of the subtype relation.

\\[S <: Top\\]
Subtype relation is least relation closed under the rules we have given. Reflexivity
and transitivity means the subtype relation is a preorder, but it is not a partial order
because of the permutation rule.

###Properties of Subtyping and Typing

Lemma Inversion of the Subtype Relation

1. If $S <: T_1 \to T_2$, then S has the form $S_1 \to S_2$, with $T_1 <: S_1$ and $S_2 <: T_2$.
2. If $S <: {l_i : T_i^{i \in 1..n}}$, then S has the form ${k_j:S_j^{j \in 1..m}}$ with at
least the labels ${l_i^{i \in 1..n}}$ and with $S_j <: T_i$ for each common label
$l_j = k_j$.

Lemma Substitution
If $\Gamma,x:S \vdash t:T$ and $\Gamma \vdash s :S$, then $\Gamma \vdash [x \mapsto s]t : T$

Theorem Preservation
If $\Gamma \vdash T$ and $t \to t', then $\Gamma \vdash t' : T$.

Lemma Canonical Forms

1. If v is a closed value of type $T_1 \to T_2$, then v has the form $\lambda x : S_1 . t_2$.
2. If v is a closed value of type ${l_i : T_i^{i \in 1..n}}$, then v has the form
${k_j=v_j^{a \in 1..m}}$, with ${l_i^{i \in 1..n}} \subset {k_a^{a \in 1..m}}$

Theorem Progress
If t is a closed, well-typed term, then either t is a value or there is some $t'$
with $t \to t'$.

###Top and Bottom Types
Top is not necessary for simply typed lambda-calculus, it can be removed,
but it is very useful.

It corresponds to the type $Object$ found in many OOP languages.
Top is a convenient device for subtyping and parametric polymorphism.
In System $F_{<:}$, the presence of Top allows one to recover ordinary
unbounded quantification from bounded quantification.

Bot is a minimal type that can subtype every type. Bot is empty, there are no
closed values of type Bot. Signals to the user that no result is expected,
signals to the typechecker that such an expression can safely be used in a
context expecting any type of value. However, Bot makes the typechecker system
more diffiult.

###Subtyping and Other features
###Ascription and Casting
Ascription operator t as T allows the programmer to record in the text of the
program the assertion that some subterm of an expression has a particular type.

In langauges like Java and C++, casting is a form of ascription, writtens as
`(T)t`. Up-casts are straightforward and down-casts require dynamice type-testing
which requires significant extension to type system.

Up-cast: ascribe a term to its supertype. A form of abstraction, hide the existence
of some parts of a value so they cannot be used in some other context.

T-Ascribe

\\[
\\frac{\\Gamma \\vdash t_1 : T}
      {\\Gamma \\vdash t_1 \\; as \\; T : T}
\\]

T-Downcast (as)
\\[
\\frac{\\Gamma \\vdash t_1 : S}
      {\\Gamma \\vdash t_1 \\; as \\; T : T}
\\]

Downcast as a poor-man's polymorphism.

###Coercion Semantics for Subtyping
The presence of subtyping does not change the way programs are evaluated.

$C :: S <: T$ C is a subtyping derivation tree whose conclusion is $S <: T$.
$D :: \Gamma \vdash t : T$ D is a typing derivation whose conclusion is $\Gamma \vdash t : T$.

Coercion is [C]. A function from type [S] to type [T]

If $C :: S <: T$, then $\vdash [C] : [S] \to [T]$
If D is a derivation of the statement $\Gamma \vdash t : T$ then its translation
$[\mathbb{D}]$ is a target-language term of type [T].

Coherence

A translation [] from typing derivations in one language to terms in another or
coherent if, for every pair of derivations D_1 and D_2 with the same conclusion


### Intersection and Union Types
