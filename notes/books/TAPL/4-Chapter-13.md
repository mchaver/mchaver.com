---
title: Chapter 13 - References
date:  2016-07-10
---

Mutable References.
Deal with the heap.
variable $x$ with the value 6
variable $y$ which is a reference (pointer) to a mutable cell whose current
container is 5.

allocation: provide initial value for a cell
dereferencing: get the value of a cell
assignment: change the value of a cell

result of assignmnet is the value unit.

$(\lambda _ : Unit. !r) (r := succ(!r))$

if you copy a reference, the reference gets copied, not the cell.

$Ref(Nat \to Nat)$

storage deallocation, run-time system handles it in ML

Types
T-Ref
\\[
\\frac{\\Gamma \\vdash t_1 : T_1}
      {\\Gamma \\vdash ref \\; t_1 : Ref \\; T_1}
\\]

T-Deref
\\[
\\frac{\\Gamma \\vdash t_1 : Ref \\; T_1}
      {\\Gamma \\vdash !t_1 : T_1}
\\]

T-Assign
\\[
\\frac{\\Gamma \\vdash t_1 : Ref \\; T_1 \\qquad \\Gamma \\vdash t_2 : T_1}
      {\\Gamma \\vdash t_1 := t_2 : Unit}
\\]

run-time store for reference is a big array of bytes.
array of values, abstract away from idea that references are numbers.
Elements of an uninterpreted set $\mathbb{L}$ of store locations, store is
a partial function from locations l to values. metavariable $\mu$ to range over
stores (heaps). A reference then is a location, an abstract index into the store.

operational semantics

E-AppAbs (apply function without changing the store $\mu$, function application
  has no side effects)
$(\lambda x : T_{11} . t_{12})v_2 | \eta \to [x \mapsto v_2] t_{12} | \mu$

E-App1
\\[
\\frac{t_1 | \\mu \\to t_1' | \\mu '}
      {t_1 t_2 | \\mu \\to t_1' t_2 | \\mu '}
\\]

E-App2
\\[
\\frac{t_2 | \\mu \\to t_2' | \\mu '}
      {v_1 t_2 | \\mu \\to v_1 t_2' | \\mu '}
\\]

E-Deref (reduce $t_1$ until it becomes a value until it becomes a value)
\\[
\\frac{t_1 | \mu \to t_1' | \mu '}
      {!t_1 | \mu \to !t_1' | \mu '}
\\]

E-DerefLoc

\\[
\\frac{\mu (l) = v}
      {!l | \mu \to v | \mu}
\\]

E-Assign1 (evaluate $t_1$ until it becomes a value (a location) then assign $t_1 := t_2$)

\\[
\\frac{t_1 | \mu \to t_1' | \mu '}
      {t_1 := t_2 | \mu \to t_1' := t_2 | \mu '}
\\]

E-Assign2 (evaluate $t_2$ until it becomes a value)

\\[
\\frac{t_2 | \\mu \\to t_2' | \\mu '}
      {v_1 := t_2 | \\mu \\to v_1 := t_2' | \\mu '}
\\]

E-Assign
\\[
l:=v_2 | \mu \to unit | [l \mapsto v_2] \mu
\\]

E-Ref
\\[
\\frac{t_1 | \mu \to t_1' | \mu '}
      {ref \; t_1 | \my \to ref \; t_1'| \mu '}
\\]

E-RefV

\\[
\\frac{l \notin dom(\mu)}
      {ref \; v_1 | \mu \to l | (\mu ,l \mapsto v_1)}
\\]

###Store Typings

What is the type of a location?

The type of a result depende os on the contents of the store.

Location typing rule

\\[
\\frac{\\Gamma \\vdash \\mu (l) : T_1}
      {\\Gamma \\vdash l : Ref \\; T_1}
\\]

To find the type of location $l$, look up the current contents of $l$ and calculate
the type $T_1$ of the contents.

New typing rule for references

\\[
\\frac{\\Gamma | \\mu \\vdash \\mu (l) : T_1}
      {\\Gamma | \\mu \\vdash l : Ref \\; T_1}
\\]

Two problems: type checking is inefficient, calculating the type of a location $l$
involves calculating the type of the current contents $v$ of $l$. It also may not
allow us to derive anything if the store contains a cycle (two references refer
to each other).

Suppose a store typing $\Sigma$ describes the store $\mu$ in which some term $t$
will be evaluated. Use $\Sigma$ to calculate the type of the result of $t$ without
ever looking directly at $\mu$.

\\[
\\frac{\\Sigma (l) = T_1}
      {\\Gamma | \\Sigma \\vdash l : Ref \\; T_1}
\\]


###Safety
Standard type safety properties should hold for a calculus with references.

A store $\mu$ is well typed with respect to a typing context $\Gamma$ and a store
typing $\Gamma$, written as $\Gamma | \Sigma \vdash \mu$, if $dom(\mu) = dom(\Sigma)$
and $\Gamma | \Sigma \vdash \mu (l) : \Sigma(l)$ for every $l \in dom(\mu)$.
