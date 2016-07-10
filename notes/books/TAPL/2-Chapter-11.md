---
title: Chapter 11 - Simple Extensions
date:  2016-07-10
---
$\mathcal{A}$ : atomic types or base types and uninterpreted or unknown base types with no primitive operations on them.
\\[
\\begin{array}{lr}
\\text{T ::= ...} & types:\\\\
\\quad A & base \\; type\\\\
\\end{array}
\\]

Uninterpreted type: no element, no constructor, but it can still have variables
bound over the elements of a base type.

$\lambda x : A. x;$ is an identity function of type $A \to A$.

$\lambda f : A \to A.\lambda x : A. f(f(x))$ is a function of type
$(A \to A) \to A \to A$ that repeats the behavior of some function $f$ on
an argument $x$.

Unit type: has a single element $unit$. It is possible reulst value of copmutations.
Its main application is for side effects. $\Gamma \; \vdash \; unit:Unit$

Sequencing notation: $t_1 \; ; \; t_2$, in languages with side effects, this
means interpret $t_1$, throw away its result, and evaluate $t_2$.

####Sequencing is a Derived Form
$e \in \lambda^E \to \lambda^I$ is the elaboration function that translates from
the external to the internal language by replacing every occurrence of $t_1;t_2$
with $(\lambda x : Unit. t_2)t_1$, where $x$ is chosen fresh in each case. For
each term $t$ in $\lambda^E$

 - $t \to_E t' \; iff \; e(t) \to_I e(t')$
 - $\Gamma \; \vdash^E \; t : T \; iff \; \Gamma \; \vdash^I e(t) : T$

Derived forms are often called syntactic sugar. It shows that the typing and
evaluation behavior of the sequencing construct can be derived from those of
the more fundamental operations of abstraction and application.

Ascription: ascribe a term to a type. $t \; as \; T$ means for the term t, to which
we ascribe the type T.

Ascription example  
$UU \; = \; Unit \to Unit$  
$\lambda f: Unit \to Unit. f;$  
$\lambda f: UU. f;$  

Let bindings: give names to subexpressions.
call-by-value evaluation order
$let x = t_1 \; in \; t_2$ is the same as $(\lambda x : T_1 . t_2)t_1$

####Pairs  
product, cartesian product: $T_1 \times T_2$

####Tuples  
a collection of n-ary values. Need index notation for the type theory to be able
to discuss general tuples of any length. Behavior is similar to pairs but need to
account for index of the type.

Tuple projection evaluation: $\{l_i = v_i ^{i \in 1..n}\}. l_j \to v_j$

Tuple projection type:

\\[
\\frac{\\Gamma \\; \\vdash \\; t_1 : \\{ l_i : T_i^{i \\in 1 .. n}\\}}
      {\\Gamma \\; \\vdash \\; t_1 . l_j : T_j}
\\]

####Records  
Similar to tuples, but they add tags for the values. It requires a matching rule
to retrieve the value.

####Sums  
Heterogenous collection of values. This is supported by variant types.

####Variants  
Use named labels instead of $inl$ and $inr$.

####Options  
Like Haskell Maybe type. Here $Nothing$ is presented as $Unit$ type.

####Enumerations  
Degenerate version of variant types. We are only interested in the label but not
any type being returned. Tags that are all of type Unit.

####General Recursion
Pass a higher-order function to $fix$ that generates funcitons.
Need to add $fix$ as a primitive with evaluation rules mimicking
the behavior of the untyped $fix$ function and a typing rule that
captures its intended use.
