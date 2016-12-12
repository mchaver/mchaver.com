---
title: The Calculus of Constructions
date: 2016-08-03
---
A system with all four possible choices: "terms/types depending on terms/types".
$\lambda C = \lambda 2 + \lambda \omega + \lambda P$. The difference is the
formation rule:

$\lambda P$ formation rule requires that $A$ must have type $\ast$ in order
to guarantee that the inhabitants of $\Pi x : A . B$ are terms or types
dependent on terms only.

In $\lambda C$ set $A : s$ with $s$ in ${\ast, \box}$, where is $s$ is indexed
so they can be chosen independently.

\\[
\\frac{\\Gamma \\vdash A : s_1 \\quad \\Gamma, x : A \\vdash B : s_2}
      {\\Gamma \\vdash \\Pi x : A . B : s_2}
\\]

The premiss has $s_1$ and $s_2$, the conclusion has $s_2$. The type of $\Pi x : A . B$
is inherited from its body.

1. $(\ast,\ast)$, term depending on term, $\lambda \to$
2. $(\box,\ast)$, term depending on type, $\lambda 2$
3. $(\box,\box)$, type depending on type, $\lambda \omega$
4. $(\ast,\box)$, type depending on term, $\lambda P$

####The lambda cube

All eight types can be described with a set of simple rules and slight variations.

Initialization rules: sort, var, weak.
Formation rule for Pi types
Conversion rules
Application
Abstraction
