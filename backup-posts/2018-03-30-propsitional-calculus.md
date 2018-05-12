---
title: Propositional Calculus
tags: logic, notes
---

axiom/postulate is a statement that is taken to be true

$P \bigvee Q$ P or Q, disjunction of P and Q
$P \bigwedge Q$ P and Q, conjuntion of P and Q
$\neg P$ negation of P

$$\frac{P \bigvee Q \\ \neg Q}{\therefor P}$$

Propositional Calculus (Sentential Calculus)
Propositions that are formed of other propositions that are connected with logical connectives.

If P then Q. P, Q. 
$ P \implies Q, P \vdash Q$

Syntactical transformation rules one can use to infer a conclusion from a a premise to create an argument

$\implies$

\bigvee or
\bigwedge and
\neg not

DeMorgan's law

$\neg (P \bigwedge Q)$ is $\neg P \bigvee \neg Q$.
$\neg (P \bigvee Q)$ is $\neg P \bigwedge \neg Q$.

Commutative laws

$P \bigwedge Q$ is $Q \bigwedge P$.
$P \bigvee Q$ is $Q \bigvee P$.

Associative laws

$P \bigwedge (Q \bigwedge R)$ is $(P \bigwedge Q) \bigwedge R$.
$P \bigvee (Q \bigvee R)$ is $(P \bigvee Q) \bigvee R$.

Idempotent laws

$P \bigwedge P$ is $P$.
$P \bigvee P$ is $P$.

Distributive laws

$P \bigwedge (Q \bigvee R)$ is $(P \bigwedge Q) \bigvee R$.
$P \bigvee (Q \bigwedge R)$ is $(P \bigvee Q) \bigwedge R$.

Absorption laws

$P \bigvee (P \bigwedge Q)$ is $P$.
$P \bigwedge (P \bigvee Q)$ is $P$.

Double Negation law

$\neg \neg P$ is $P$.

Tautology law

$P \bigwedge$ (a tautology) is $P$.
$P \bigvee$ (a tautology) is tautology.
$\neg$ (a tautology) is a contradiction.

Contradiction law

$P \bigwedge$ (a contradiction) is a contradiction.
$P \bigvee$ (a contradiction) is $P$.
$\neg$ (a contradiction) is a tautology.

Formulas that are always true are tautologies $P \bigvee \neg P$.
Formulas that are always false are contradictions $P \bigwedge \neg P$.

## Law of Excluded Middle

For any proposition, it is either ture or its negation is true.

## Principle of non-contradiction
Negation Introduction
Proof by contradiction
Reductio ad absurdum
Disprove a statement by showing it leads to an absurd conclusion.
A statement cannot be both true and false. If a proposition $Q$ and its negation $\neg Q$ can both be derived from a premise, then it can be concluded that the premise is false.

$$\frac{\varphi \vdash \psi \\ \varphi \vdash \neg \psi}{\neg \varphi}$$


\bigcup
$A \bigcap B$ things that are in both A and B.
$A \bigcup B$ things that are in either A or B.
$A \ B$ things that are only in A.

$(A \bigcup B) \ (A \bigcap B)$ is $(A \ B) \bigcup (B \bigcup A)$ things that are only in A and only in B but not in both.

Conditional and Biconditional Connective
if P then Q $P \implies Q$

$$\frac{P \implies Q \\ P}{\therefore Q}$$

Conditional law
$P \implies Q$ is $\neg P \bigvee Q$.
$P \implies Q$ is $\neg (P \bigwedge \neg Q)$.

Contrapositive law
$P \implies Q$ is $\neg Q \implies \neg P$.

iff if and only if $P \iff Q$

## Formal Description

Propositional Calculus is a system $L = L(A, \Omega, Z, I)$.

- The alpha set A is a finite set of proposition symbols. These are the terminal elements of $L$.
- The omega set $\Omega$ is a finite set of elements called logical connectives (operator symbols). $\Omega_1 = {\neg}$ and $\Omega_2 = {\bigwedge , \bigvee , \implies, \iff }$.
- The zeta set $Z$ is a finite set of transformation rules (inference rules).
- The iota set $I$ is a finit set of initial points called axioms.

The language $L$ is inductively defined by the rules:
1. Any element of the alpha set $A$ is a formula of $L$.
2. If $p_1, p_2, ..., p_j$ are formulas and $f$ is in $\Omega_j$ then $(f(p_1, p_2, ...,p_j))$ is a formula.
3. Nothing else is formula of L.




https://en.wikipedia.org/wiki/List_of_rules_of_inference

https://en.wikipedia.org/wiki/Law_of_thought#The_three_traditional_laws

https://en.wikipedia.org/wiki/Axiom
https://en.wikipedia.org/wiki/Propositional_calculus
How to Prove It chapter 2