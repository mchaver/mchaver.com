---
title: Chapter 9 - Simply Typed Lambda-Calculus
date:  2016-07-09
---

The grammar of a set of simple types over the type Bool

```
    T ::=                  types:
      Bool       type of booleans
      T -> T    type of functions
```

The type constructor $\to$ is right-associative. $T_1 \to T_2 \to T_3$ is
equivalent to $T_1 \to (T_2 \to T_3)$.

__Explicitly typed__: type annotations are used to guide the typechecker.  
__Implicitly typed__: the typechecker infers or reconstructs the types.

The general form of typing abstractions

\\[
\\frac{\\Gamma , x:T_1 \\vdash t_2 : T_2}
      {\\Gamma \\vdash \\lambda x : T_1 . t_2 : T_1 \\to T_2}
\\]

$x$ is of type $T_1$ and the body $t_2$ is of type $T_2$, then we know the type
of the function is $T_1 \to T_2$.

The typing rule for variables

\\[
\\frac{x : T \\in \\Gamma}
      {\\Gamma \\vdash x : T}
\\]

the type assumed for $x$ in $\Gamma$ is $T$.

The typing rule for applications

\\[
\\frac{\\Gamma \\vdash t_1 : T_{11} \\to T_{12} \\qquad \\Gamma \\vdash t_2 : T_{11}}
      {\\Gamma \\vdash t_1 t_2 : T_{12}}
\\]

if $t_1$ evaluates to a function mapping arguments in $T_{11}$
to results in $T_{12}$ and if $t_2$ evaluates to a result in
$T_{11}$ then the result of applying $t_1$ to $t_2$ will be a
value of type $T_12$.

###Properties of Typing

####Inversion Lemma

Records a collection of observations about how typing derivations are built,

1. If $\Gamma \vdash x : R$, then $x : R \in \Gamma$.
2. If $\Gamma \vdash \lambda x : T_1 . t_2 : R$, then $R = T_1 \to R_2$ for some $R_2$ with $\Gamma , x : T_! \vdash t_2 : R_2$.
3. If $\Gamma \vdash t_1 \; t_2 : R$, then there is some type $T_{11}$ such that $\Gamma \vdash t_1 : T_{11} \to R$ and $\Gamma \vdash t_2 : T_{11}$.
4. If $\Gamma \vdash true : R$, then $R = Bool$.
5. If $\Gamma \vdash false : R$, the $R = Bool$.
6. If $\Gamma \vdash if t_1 then t_2 else t_3 : R$, then $\Gamma \vdash t_1 : Bool$ and $\Gamma \vdash t_2,t_3 : R$

####Uniqueness of Types Theorem

In a given $\Gamma$, a term $t$ has at most one type. If a term is typable, then its type is unique. There is just one derivation of this typing built from the inference rules that generate the typing relation.

####Canonical Forms Lemma

1. If $v$ is a value of type $Bool$, then $v$ is either $true$ or $false$.
2. if $v$ is a value of type $T_1 \to T_2$, then $v = \lambda x : T_1. t_2$

####Progress Theorem

Suppose that $t$ is closed, well-type term $\vdash t : T$, then either $t$ is a value or else there is some $t'$ with $t \to t'$.

####Permutation Lemma

If $\Gamma \vdash t : T$ and $\Delta$ is a permutation of $\Gamma$, the $\Delta \vdash t : T$. The latter derivation has the same depth as the former.

####Weakening Lemma

If $\Gamma \vdash t : T$ and $x \notin dom(\Gamma)$, then $\Gamma , x : S \vdash t : T$.

####Preservation of Types Under Substitution Lemma

If $\Gamma , x : S \vdash t : T$ and $\Gamma \vdash s : S$, then $\Gamma vdash [x -> s]t : T$

####Preservation Theorem

If $\Gamma \vdash t : T$ and $t \to t'$, then $\Gamma \vdash t' : T$

####The Curry-Howard Correspondence

$\to$ type constructor has two kinds of typing rules:

1. an introduction rule describing how elements of the type can be created.
2. an elimination rule describing how elements of the type can be used.

A proof of a proposition P consists of concrete evidence for P. Curry and Howard noticed that such evidence has a computational fell. Proof $P \subset Q$ is a mechanical procedure that constructs a proof of Q given P.

| Logic | Programming Languages
|---|---|
| propositions | types |
| proposition $P \subset Q$ | type $P \to Q$|
| proposition $P \wedge Q$ | type $P \times Q$|
| proof of proposition P | term t of type P|
| proposition P is provable | type P is inhabited by some term|

Computation (reduction of lambda terms) corresponds to the logical operation of proof simplification by cut elimination.

###Erasure and Typability

Most compilers avoid carrying annotations at runtime. They are used during typechecking but do not appear in the compiled form of the program.

Erasure of a simply typed term t is defined as follows:  
$erase(x) \; = \; x$   
$erase(\lambda x : T_1 . t_2) \; = \; \lambda x . erase (t_2)$  
$erase(t_1 \; t_2) \; = \; erase(t_1) erase(t_2)$  

####Erasure Theorem

1. If $t \to t'$ under the typed evaluation relation, then $erase(t) \to erase(t')$
2. If $erase(t) \to m'$ under the typed evaluation relation, the there is a simply typed term $t'$ such that $t \to t'$ and $erase(t') = m'$.

A term $m$ in the untyped lambda-calculus is said to be typable in $\lambda_{\to}$ if there are some simply typed term $t$, type $T$, and context $\Gamma$ such that $erase(t) = m$ and $\Gamma \vdash t : T$.
