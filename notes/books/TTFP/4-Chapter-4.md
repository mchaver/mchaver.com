---
title: Types dependent on types
date: 2016-07-27
---
$\lambda \omega$

Types depending on types.

####Type constructors

$\beta \to \beta$, $\gamma \to \gamma$, etc. all have the general structure
$\Diamond \to \Diamond$. Abstracting over $\Diamond$ makes it possible to
describe the whole family of types with this structure.

Type constructor: $\lambda: * . \alpha \to \alpha$ function with a type as a value.

$(\lambda \alpha : * . \alpha \to \alpha) \beta \; \to_{\beta} \; \beta \to \beta$

$(\lambda \alpha : * . \alpha \to \alpha) (\gamma \to \beta) \; \to_{\beta} \;  (\gamma \to \beta) \to (\gamma \to \beta)$

Type of constructors: $\lambda \alpha : * . \alpha \to \alpha : * \to \ast$.

Kinds: $\lambda \alpha : * . \lambda \beta : * . \alpha \to \beta : * \to (* \to * )$

$\mathbb{K} = * \; | \; (\mathbb{K} \to \mathbb{K})$.

Type of all kinds: $\square$, thus $* : \square$ or $* \to * : \square$. If k
is a kind, then often each M of type K is a type constructor.

Sort: $\ast$ or $\square$

1. If $k : \square$ and $M : k$, then M is a constructor. If k is not $\ast$, then
M is a proper constructor.
2. Thee set of sorts is $\{\ast ,\square \}$.

Four levels

1. Terms
2. Constructors (types plus the proper constructors)
3. Kinds
4. $\square$

Glue these together to write judgement chains like $t : \sigma : \ast \to \ast$
or $t : \sigma : \ast \to \ast : \square$, expressing $t : \sigma$ and $\sigma : \ast \to \ast$
and $\ast \to \ast : \square.$

####Sort-rule and var-rule

sort-rule: $\emptyset \vdash * : \square$

var-rule:
\\[
\\frac{\\Gamma \\vdash A : s}
      {\\Gamma, x : A \\vdash x : A} \\;
if \\; x \\notin \\Gamma      
\\]

The premiss of var-rule requires that A itself is a type (if f is $\ast$) or a kind
(if s is $\square$). The restriction $x \notin \Gamma$ guarantees that variable
x is fresh (does not occur in $\Gamma$).

####Weakening rule
Weaken the context of a judgement by adding new declarations, provided that the
types of the new declarations are well-formed.

Weakening rule:
\\[
\\frac{\\Gamma \\vdash A : B \\qquad \\Gamma \\vdash C : s}
      {\\Gamma, x : C \\vdash A : B} \\;
if \\; x \\notin \\Gamma      
\\]

Add arbitrary declaration at the end, and $A : B$ is still derivable.

Thinning: process of inserting a new declaration in a list of declarations at
an arbitrary place.

Weakening: inserting a new declaration at the end.
