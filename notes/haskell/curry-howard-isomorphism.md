---
title: The Curry-Howard Isomorphism
date:  2016-07-12
---

Curry-Howard Isomorphism is a relation between type theory and structural logic.
LaTeX text corresponds to Curry-Howard Isomorphism, code text (between backticks
in markdown) correspond to Haskell code.

To prove a mathematical theorem, construct a certain type which reflects the nature
of that theorem, then find a value that has that type. Given an arbitrary type,
under what conditions does there exist a value with that type. If a value with
that type exists then it is inhabited.

There is no function with type `a -> b` because there is no way of turning something
of type `a` into type `b` unless we know which types `a` and `b` are in advance.
A type is only inhabited when it corresponds to a true theorem in mathematical
logic.

####Formal Logic Quick Introduction
$A \to B$ is read as "A implies B". B is true whenever A is true.
$\to$ does not represent a cause-effect relationship, $P \to Q$ is defined to be true
whenever P is false and whenever Q is true.

theorems: true statements
nontheorems: false statements
proposition: symbol string

####Propositions are types
$a \to b$ is a theorem iff `a -> b` is an inhabited type.
`const` has a type `a -> b -> a`. In logic this is $a \to b \to a$. This is a
theorem because the type `a -> b -> a` is inhabited by `const`.

####Problem with ⊥
In Haskell every type is inhabited with the value `undefined`. ⊥ corresponds to
an inconsistency in logic. We can prove any theorem using Haskell types because
every type is inhabited. Haskell's type system is thus an inconsistent logic
system.

####Logical operations

The way of combining propositions (P, Q) are operations on propositions ($\vee$, $\to$).
Propositions correspond to types, proposition combinators are type constructors (type operations).
Implication operator $\to$ corresponds to the type constructor `(->)`.

####Conjunction and Disjunction
For $A \wedge B$ to be a theorem, both $A$ and $B$ must be theorems. A value with two
subvalues in Haskell is `(a,b)`. For $A \vee B$ to be a theorem, either $A$ or $B$
must be theorems, in Haskell `Either A B`

####False statements
A false statement is one that cannot be proven, an uninhabited type `data Void`.
This omits constructors.

1. `(Void,A)` and `(A,Void)` are both uninhabited types for any `A`, because both need to be theorems.
2. `Either Void A` and `Either A Void` are theorems iff `A` is an inhabited type.
3. Any type that corresponds to a nontheorem can be replaced with `Void`.
4. Implication $P \to Q$ is true if $Q$ is true, regardless of the truth value
of $P$. We should be able to find a term with type `Void -> a`, the empty function.
`f :: A -> B` as a set of paris whose first element is an element of `A` and
second element if `f`'s output on this term, an element of `B`. The successor function
on natural numbers is `{(0,1),(1,2),(2,3),...}`. `empty` is represented by an
empty set, it has no pair. The domain type must be empty, `Void`. `empty` never
produces output, so there are no restrictions placed on the range type,
```haskell
empty :: Void -> a
empty _ = undefined
```
or
```haskell
empty :: Void -> a
empty = empty
```

####Negation

¬ operation turns theorems into nontheorems and vice versa.
`type Not a = a -> Void`

####Axiomatic Logic
Two axioms for the behavior of $\to$

1. $A \to B \to A$.
2. $(A \to B \to C) \to (A \to B) \to A \to C$.
3. If $A \to B$, and $A$, then $B$.

(1) Any two propositions, if we assume both $A$ and $B$, we know that $A$ is true.

(2) If $A$ implies that $B$ implies $C$ (C is true whenever A and B are true), and
A implies B, then knowing A is true would be enough to conclude that C is true.

A = The box is red
B = The box has wheels
C = The box has a lid

$A \to B \to C$ All red boxes with wheels have lids.
$A \to B$ All red boxes have wheels
if $A$ if the box is red
then $C$ then the box has a lid

(3) Create new theorems given old ones. Inference law or modus ponens.

####Combinator Calculus

Lambda calculus is a simple programming language defined from a simple base.

1. A value $v$.
2. A lambda abstraction $\lambda x . t$, $t$ is another lambda term.
3. An application $(t_1 t_2)$, $t_1$ and $t_2$ are lambda terms.

Beta-reduction: $((\lambda x . t_1) t_2 ) \to t_1 [x := t_2]$, $t_1 [x := t_2]$
means $t_1$ with all the free occurrences of $x$ replaced with $t_2$.

Combinators

1. K takes two values and returns the first. $K = \lambda x y . x .$ same as
`const`.
2. S takes a binary function, a unary function and a value, and applies that value
and the value passed into the unary function to the binary function.
$S = \lambda x y z. xz(yz)$, `ap` in the `((->) e)` Reader monad. 






















Inhabited type: Type that has a value.
