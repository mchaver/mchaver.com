---
title: Chapter 23 - Universal Types
date:  2016-07-18
---

Parametric Polymorphism: single piece of  code to be typed generically, use
variables instead of types, instantiated with particular types as needed.  

First-Class Polymorphism (Impredicative Polymorphism):

Ad-hoc polymorphism: allows polymorphic value to exhibit different behaviors
when viewed at different types. Most common form is function overloading.

multi-method dispatch

intensional polymorphism permits restricted computation over types at run time.
Allows features such as tag-free garbage collection, unboxed function arguments
polymorphic marshaling and space-efficient flattened data structures.

subtype polymorphism: gives a single term many types using the rule of subsumption
allowing us to selectively forget information about the term's behavior.

categories are not mutually exclusive.

###System F

An extension of simply typed lambda-calculus. In $\lambda_\to$ lambda-abstractions
is used to abstract terms out of terms and application is used to supply values for
the abstracted parts.

$\lambda X . t$ abstract types out of terms. (type abstractions)
$t[T]$ form application, argument is a type expression. (type application, instanctiation)

E-TappTabs
$(\lambda X . t_{12}) [T_2] \to [X \mapsto T_2] t_{12}$

E-AppAbs
$(\lambda x : T_{11} . t_{12}) v_2 \to [x \mapsto v_2] t_{12}$

polymorphic identity function $id \; = \; \lambda X . \; \lambda x : X. \; x;$
apply to $Nat$ by writing $id \; [Nat]$, the result is $[X \mapsto Nat]( \lambda x : X.x)$
$\lambda x : Nat . x$

Need a new arrow type
id depends on the type that we pass to it as an arugment $\forall X . X \to X$

T-Abs

\\[
\\frac{\\Gamma , X \\vdash t_2 : T_2}
      {\\Gamma \\vdash \\lambda X . t_2 : \\forall X . T_2}
\\]

T-TApp

\\[
\\frac{\\Gamma \\vdash t_1 : \\forall X . T_{12}}
      {\\Gamma \\vdash t_1 [T_2] : [X \mapsto T_2] T_{12}}
\\]

###Examples
\\[
id = \\lambda X . \\; \\lambda x : X. x; \\
id \\; [Nat];
id \\; [Nat] \\; 0;
\\]

double, run a function twice
\\[
double \\; = \\; \\lambda X. \lambda f: X \\to X. \\lambda a : X. f (f a); \\
doubleNat \\; = \\; double \\; [Nat]; \\
doubleNatArrowNat \\; = \\; double \\; [Nat \\to Nat]; \\
double [Nat] (\\lambda x : Nat. \\; succ(succ(x))) 3;
\\]
