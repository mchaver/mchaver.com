---
title: Simple typed lambda calculus
date: 2016-07-26
---

Infinite set of type variables: $\mathbb{V} = \{ \alpha,\beta,\gamma,\ldots \}$.

The set $\mathbb{T}$ of all simple types:

1. Type variable: If $\alpha \in \mathbb{V}$, then $\alpha \in \mathbb{T}$,
2. Arrow type: If $\sigma, \tau \in \mathbb{T}$, then $(\sigma \to \tau) \in \mathbb{T}$

Abstract syntax version: $\mathbb{T} = \mathbb{V} \; | \; \mathbb{T} \to \mathbb{T}$.

Type statement: $M : \sigma$.

Application: if $M : \sigma \to \tau$ and $N : \sigma$, then $MN : \tau$.

Abstraction: if $x : \sigma$ and $M : \tau$, then $\lambda x . M : \sigma \to \tau$.

Typable term: A term $M$ is typable if there is a type $\sigma$ such that $M : \sigma$.

####Church-typing and Curry-typing

There are two ways to give types to variables:

1. Church-typing (explicit typing): Prescribe a unique type for each variable
upon its introduction.

2. Curry-typing (implicit typing): Not give the types of variables. Typable
terms are found by a search process.

####Derivation rules for Church's $\lambda \to$

Pre-type $\lambda$-terms, $\Lambda_{\mathbb{T}}$:

$\Lambda_{\mathbb{T}} = V | (\Lambda_{\mathbb{T}}\Lambda_{\mathbb{T}}) | (\lambda V : \mathbb{T} . \Lambda{\mathbb{T}})$.

1. Statement: $M : \sigma$, where $M \in \Lambda_{\mathbb{T}}$ and $\sigma \in \mathbb{T}$.
$M$ is the subject and $\sigma$ is the type.
2. Declaration: variable as the subject.
3. Context: list of declarations with different subjects.
4. Judgement: $\Gamma \vdash M : \sigma$, $\Gamma$ is a context and $M : \sigma$
is a statement.

Derivation system: each rule explains how certain judgements can be formally
established. Premiss-conclusion format. Premisses above the line and conclusions
below.

Derivation rules for Church's $\lambda \to$:

(var)
\\[\\Gamma \\vdash x : \\sigma \\; if \\; x : \\sigma \\in \\Gamma\\]

(appl)
\\[
\\frac{\\Gamma \\vdash M : \\sigma \\to \\tau \\quad \\Gamma \\vdash N : \\sigma}
      {\\Gamma \\vdash M N : \\tau}
\\]

(abst)
\\[
\\frac{\\Gamma, x: \\sigma \\vdash M : \\sigma}
      {\\Gamma \\vdash \\lambda x : \\sigma . M : \\sigma \\to \\tau}
\\]

Legal: terms that are typable with the aid osf a derivation system. A pre-typed
term $M$ in $\lambda \to$ is called legal if there exist context $\Gamma$ and
type $\rho$ such that $\Gamma \vdash M : \rho$.

####Kinds of problems to be solved in type theory

Well-typedness (typability): $? \vdash term : ?$ find out whether a term is
legal. Find an appropriate context and type if the term is legal, if not, show
where it is wrong.

Type Assignment: $context \vdash term : ?$.

Type Checking: $context \vdash term : type$? Check that a certain term has a
certain type.

Term Finding (Term Construction, Inhabitation): $context \vdash ? : type$


####General properties of $\lambda \to$

1. Domain: $\Gamma$ equiavlent to $x_1 : \sigma, \ldots, x_n : \sigma_n$, then
the domain of $\Gamma$ ($dom(\Gamma)$) is the list $(x_1,\ldots,x_n)$.
2. Subcontext: context $\Gamma'$ is a subcontext of context $\Gamma$ if all declarations in
$\Gamma'$ also occur in $\Gamma$ in the same order.
3. Permutation: context $\Gamma'$ is a permutation of context $\Gamma$, if all declarations
in $\Gamma'$ also occur in $\Gamma$ and vice versa.
4. Projection: if $\Gamma$ is a context and $\Phi$ a set of variables, then the projection
of $\Gamma$ on $\Phi$, or $\Gamma \upharpoonright \Phi$, is the subcontext $\Gamma'$
of $\Gamma$ where $dom(\Gamma') = $dom(\Gamma) \cap \Phi$

Free variable lemma:
If $\Gamma \vdash L : \sigma$ then $FV(L) \subset dom(\Gamma)$.
Each free variable x that occurs in L has a type.

1. Thinning: Let $\Gamma'$ and $\Gamma''$ such that $\Gamma' \subseteq \Gamma''$.
If $\Gamma' \vdash M : \sigma$ then also $\Gamma'' \vdash M : \sigma$.
2. Condensing: If $\Gamma \vdash M : \sigma$ , then $\Gamma \upharpoonright FV (M) \vdash M : \sigma$.
3. Permutation: If $\Gamma \vdash M : \sigma$, and $\Gamma'$ is a permutation of
$\Gamma$, then $\Gamma'$ is a context and $\Gamma' \vdash M : \sigma$.
