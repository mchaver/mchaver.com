---
title: Chapter 30 - Higher-Order Polymorphism
date:  2016-07-20
---

System $F_\omega$ is a combination of System F and $\lambda_\omega$.

####Lemma - Strengthening
If $\Gamma,x: S, \Delta \vdash T :: K$, then $\Gamma, \Delta \vdash T :: K$.

The kinding relation does not refer to term variable bindings.

####Lemma - Permutation and Weakening
Suppose contexts $\Gamma$ and $\Delta$ such that $\Delta$ is a well-formed
permutation of $\Gamma,\Sigma$ for some context $\Sigma$, $\Delta$ is a
permutation of extension of $\Gamma$.

####Lemma - Term Substitution
If $\Gamma,x:S,\Delta \vdash t:T$ and $\Gamma \vdash s : S$, then
$\Gamma,\Delta \vdash [x \mapsto s] t :T$

Parallel reduction (type equivalence relation), drops rules of symmetry and
transitivity. $(\lambda X :: K_{11} . T_{12}) T_2$ reduces to $[X \mapsto T_2] T_{12}$

$\Leftrightarrow ^*$ parallel reduction relation (transitive and symmetric closure).
$S \equiv T$ iff $S \Leftrightarrow ^* T$.


####transitive relation
binary relation R over a set X is transitive if whenvere an element a is related
to an element b, and b is related to an element c, then a is also related to c.

$\forall a,b,c \in X: (aRb \wedge bRc) \implies aRc$


####symmetric closure
binary relation R on a set X is the smallest symmetric relation on X that contains R.
X is a set of airports and xRy means "there is a direct flight from airport x to airport y",
the symmetric closure of R is the relation "there is a direct flight either from x to y or from y to x"

$S = R \cup \{(x,y):(y,x) \in R\}$

####symmetric relation
binary relation R over a set X is symmetric if it hold for all a and b in X that
a is related to b iff b is related to a.

$\forall a, b \in X(aRb \Leftrightarrow bRa)$

If two types are equivalent, then they share a common reduct.

####Dependent Types
Think of forms of abstractions as families of expressions indexed by other
expressions. $\lambda x : T_1 . t_2$ is a family of terms $[x \mapsto s]t_1$
indexed by terms $s$. $\lambda X :: K_1 . t_2$ is a family of terms indexed by
types, and a type operator is a family of types indexed by types.

 - $\lambda x : T_1 . t_2$ family of terms indexed by terms
 - $\lambda X :: K_1 . t_2$ family of terms indexed by types
 - $\lambda X :: K_1 . T_2$ family of types indexed by types

Lowercase are terms. Uppercase are types.

Dependent types are families of types indexed by terms.

Dependent function types $\Pi x : T_1 . T_2$ are more precise then $T_1 \to T_2$.
