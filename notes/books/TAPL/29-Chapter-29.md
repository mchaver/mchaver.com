---
title: Chapter 24 - Type Operators and Kinding
date:  2016-07-20
---

__Kinding__: well-formedness relation. Use kinds to prevent nonsense such as
$(Bool \; Nat)$.

__Currying__: treat a multiple argument function as a chain of one-argument functions.
$\lambda Y. \lambda Z . \forall X. (Y \to Z \to X) \to X$ represents a two argument
function that is a one-argument function that when applied to a type $S$ yields another
one-argument function that when applied to a type $T$ yields the type
$\forall X . (S \to T \to X) \to X$.

__Definitional equivalence__: $S \equiv T$
$(\lambda X :: K_{11} . T_{12}) \equiv [X \mapsto T_2] T_{12}$
a type-level abstraction applied to an argument is equivalent to the body of the
abstraction with the argument substituted for the formal parameter.

\\[
\\frac{\\Gamma \\vdash t : S \\quad S \\equiv T}
      {\\Gamma \\vdash t : T}
\\]

If two types are equivalent then the members of one are all members of the
other.

 - $^*$ the kind of proper types ($Bool$ and $Bool \to Bool$)
 - $^* \implies ^*$ the kind of type operators (functions from proper types to proper types)
 - $^* \implies ^* \implies ^*$ the kind of functions from proper types to type operators
 - $(^* \implies ^* ) \implies ^*$ the kind of functions from type operators to proper types (higher-order type operators)

Kinds are the types of types. The system of kinds is a copy of simply typed
lambda-calculus one level up.

Pair type defined with kinds:
\\[Pair \\; = \\; \\lambda A :: \\; ^* . \\lambda B :: \\; ^* . \\forall X. (A \\to B \\to X) \\to X;\\]

####Expressions have three classes:

__terms__: integer, float, compound data values (records), value-level abstractions,
applications, type abstractions and type applications.

__types__: $Nat$, $Nat \to Nat$, $Pair \; Nat \; Bool$, $\forall X . X \to X$
$Pair$, $\lambda X . X \to X$ do not classify terms but can be applied to type
arguments to form proper types that do classify terms.

__kinds__:

 - $^*$: $Nat$, $Nat \to Nat$, $(\lambda X . X \to X) Nat$
 - $^* \implies ^*$: $\lambda X . X \to X$, $Pair \; Nat$
 - $^* \implies ^* \implies ^*$: $Pair$

Kinding rules specify how type expressions can be combined to yield new type
expressions. $\Gamma \vdash T :: K$ for type $T$ has kind $K$ in context $\Gamma$.

Whenever a type $T$ appears in a term $\lambda x : T . t$, we must check that $T$
is well formed.


####Other types of kinds
 - record kinds
 - row kinds
 - power kinds
 - power types
 - singleton kinds
 - dependent kinds
