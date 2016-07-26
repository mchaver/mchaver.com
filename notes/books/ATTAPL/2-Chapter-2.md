---
title: Dependent Types
date: 2017-07-22
---

Dependent types are type-valued functions. Pair, when applied to two types S and
T, yields the type Pair S.. Functions which send terms to types.

###Motivation
type family of vectors: $Vector :: Nat \to * $. The kinding assertion states
that $Vector$ maps a natural number $k:Nat$ to a type. Vector k contains
vectors of length k of elements of some fixed type.

takes length n, a vlue t of type data, and return a vector n elements all set
to t. init k t has type Vector k.
$init \; : \; \Pi n \; : \; Nat. \; data \to Vector \; n$

dependent product type, Pi type. $\Pi x : S. T.$. Generalizes the arrow type.
It is the type of functions which map elements $s:S$ to elements $[x \mapsto s]T$.
result type of a function with $\Pi-type$ can vary according to the argument supplied.

Build vector with empty and cons

$cons : \Pi n : Nat. data \to Vector \; n \to Vector (n+1)$

$\Pi x : S . T$ is analogous to universal type $\forall X . T$. The type of a
term t with type $\forall X . T$ also varies with the argument supplied, but
in the case of a type abstraction, the argument is a type rather than a term.
If A is a type, then $t \; A:[X \mapsto A]T$. Only occurs with type arguments,
but in dependent type theory it may occur with term-level arguments.

Dependent typing reveals more information about the behavior of a term, which
can be exploited to give more precise typings and exclude

$first \; : \; \Pi n : Nat. Vector (n+1) \to data$

$first$ can never be applied to an empty vector, non-emptiness is expressed in
within the type system itself.

$\Pi x : S . T$ generalizes the function space $S \to T$ of simply typed
lambda calculus.
$S \to T = \Pi x : S. T$ where x does not appear free in - [ ]

$sprintf : \Pi f : Format. \; Data(f) \to String$
Format is a type of valid print formats and $Data(f)$ is the type of data
corresponding to format f.

$\Pi n : Nat. \Pi ; : Nat . Lt(l,n) \to Vector(n) \to T$

$Lt(l,n)$ is the proposition asserting that l is less than n.

$\Sigma x : A.B(x)$ is the type of pairs $(a,b)$ where $a:A$ and $b:B(a)$ and
$Id \; t_1 \; t_2$ is the type of proofs of the equality $t_1 = t_2$. Similar
to existential types.

###Pure First-Order Dependent Types
$\lambda LF$ a simplified variant of the Edinburgh LF type system. Generalizes
STLC by replacing the arrow type $S \to T$ with dependent product type
$\Pi x : S.T$ and introduces type families.

$\Gamma \subseteq \Delta$ iff $x : T \in \Gamma$ implies $x : T \in \Delta$, or
$\Gamma \in \Delta$ means that $\Delta$ is a permutation of an extension of $\Gamma$.

###Calculus of Constructions
Extends $\lambda LF$ with type Prop, elements represent propositions and datatypes
type family Prf, assign to each proposition or datatype $p : Prop$ the type
$Prf \; p$ of its proofs or members (datatypes).

###Lambda Cube

Relates various typed lambda calculi.
Pure Type Systems six typing rules to relate a large family of systems
constructed from pi-types. PTS morphisms: mapping between type systems.
Single syntactic category of tmers used to form types, terms, abstractions and
applications. Uses tokens called sorts to classify different categories of
term, within the formal system.
sorts: $* $ is the kind of all proper types
$\box$ classifies well-formed kinds.
Judgments of the form $\Gamma \vdash T : * $ replace $\Gamma \vdash :: * $
$\Gamma \vdash K : \box$ replace $\Gamma \vdash K$.

T-Pi controls formation of pi-types, by restricting which sorts are allowed
to quantify over. Restricts which lambda abstractions can be introduced by T-Abs.
$(* ,* )$ first order dependent product type. $(* ,\box)$ the kind of type families.

[Dependent type](https://en.wikipedia.org/wiki/Dependent_type)

A type whose definition depends on a value.
Dependent function: return type depends on the value of an argument.
Dependent pair: may ave a second value that depends on the first.

Given a type $A : U$ in a universe of types $U$, one may have a family of types
$B : A \to U$ which assigns a term $a : A$ a type $B(a):U". The type $B(a)$ varies
with $a$.
A function whose type of return value varies with its argument is a dependent
function and the type of this function is called dependent product type, pi-type
or dependent type.

$\Pi_{(x:A)}B(x)$ id judgementally equal to $A \to B$.

$Vec(R,n)$ fo n-tuples of real numbers, then $\Pi_{(n:N)} Vec(R,n)$ would be the
type of functions which, given a natural number n, return a tuple of real numbers of
size n.

opposite of dependent product type is the dependent pair type, dependent sum type
or sigma-type. coproduct or disjoint uninot.

$\Sigma_{(x;A)}B(x)$ captures the idea of an indexed pair, where the type of the
second pair dependet on the first
$(a,b) : \Sigma_{(x:A)} B(x)$ then $a : A$ and $b : B(a)$
