---
title: Chapter 24 - Existential Types
date:  2016-07-19
---

####Universal types

Logical intuition: (type-erasure behavior) an element of the type
$\forall X . T$ is a value that has type $[X \mapsto S]T$ for all choices of
$S$. For example, the polymorphic identity function
$\lambda X . \lambda x : X. x$ erases to the untyped identity function
$\lambda x . x$, which maps an argument from any type $S$ to a result of the
same type.

Operational intuition: an element of $\forall X . T$ is a function mapping a
type $S$ to a specialized term with type $[X \mapsto S]T$.

####Existential types

Logical intuition: an element of ${\exists X, T}$ is a value of type $[X \mapsto S]T$,
for some type $S$.

Operational intuition: an element of ${\exists X, T}$ is a pair, written
$\{^* S, t\}$, of a type $S$ and a term $t$ of type $[X \mapsto S]T$.

Think of a value $\{^* S, t\}$ of type $\{\exists X, T\}$ as a simple form of
package or module with one type component and one term component. The type $S$
is often called the hidden representation type or the witness type of the package.

The package $p \; = \; \{^* Nat, \{a=5, f=\lambda x : Nat. \; succ(x)  \}\}$
has the existential type $\{ \exists X, \{a:X, f:X \to X \}\}$ The type component
of $p$ is $Nat$ and the value component is a record containing a field $a$ of
type $X$and a field $f$ of type $X \to X$.

Packages with different hidden representation type can inhabit the same existential
type.

\\[
p2 \\; = \\; \\{^* Nat, 0 \\} \\; as \\; \\{\\exists X, X\\} \\\\
p3 \\; = \\; \\{^* Bool, true \\} \\; as \\; \\{\\exists X, X\\}
\\]

####Abstract Data Types

ADT consists of a type name $A$, a concrete representation type $T$, implementations
of some operations for creating, querying, and manipulating values of type $T$,
and an abstraction boundary enclosing the representation and operations. A large
program can be broken up into a sequence of ATD declarations. Pack and then open
style of programming.

####Existential Objects

Weak-binary operations: operate on two abstract values.

Strong-binary operations: operate on two abstract values with privileged access.

####Encoding Existentials

$\{\exists X, T \} = \forall Y . (\forall X . \; T \to Y) \to Y$

An existential package is a data value that, given a result type and a continuation,
calls the continuation to yield a final result. The continuation takes two
arguments a type $X$ and a value of type $T$.
