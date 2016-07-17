---
title: Denotational Semantics
date:  2016-07-11
---

Denotational Semantics explains how to formalize the meaning of programs. It helps
explain why `square x = x*x` is equivalent to the mathematical square function.

Denotational Semantics maps each of its programs to a mathematical object (denotation) that represents
the meaning of the program. Mathematical object for `10`,`9+1`,`2*5` and `sum [1..4]`
can be represented by the integer 10. All of these programs denote the integer 10.

Semantic Domain: collection of mathematical objects representing the meaning of
programs.

Semantic Domain is written in Oxford Bracket, double square brackets. $⟦2*5⟧ = 10$.
Denotations are compositional $⟦a+b⟧ = ⟦a⟧ + ⟦b⟧$
$⟦Integer⟧ = \mathbb{Z}$

The meaning of a purely functional language is independent from its execution.

Evaluation Strategy will dictate how long a program takes to run. Its implementation
has to respect the semantics of a language. Semantics determine how a program
must be evaluated on a machine.


`f :: Integer -> Integer` can be represented as the mathematical definition of a
function, a set of (argument,value) pairs. This is more challenging with recursive
functions (it cannot be represented as a graph). It must also support partial
functions.

⊥ Bottom: undefined value or function. In Haskell, every data type contains one
⊥ among their other elements.

Integer ⊥,0,1,-1,2,-2,...

Lifting: Adding ⊥ to a set. $\mathbb{Z}_\bot$ lifted integers.

Unit type in haskell only has two inhabitants: ⊥,()

In Haskell `undefined` denotes `⊥`.
`forall a . a` denotes ⊥.

f(n) = {1 if n is 0}, {-2 if n is 1}, {⊥ else}

For every partial function, more defined answers should yield more defined arguments.
Every concrete number is more defined then ⊥. $a \sqsubset b$ mean b is more defined
than a. $a \sqsubseteq b$ denotes that b is more defined than a or both are equal.
$\sqsubset$ is the semantic approximation order because we can approximate defined values
by less defined ones. ⊥ is the least defined element of a data type
$\forall x \noteq \bot \; \bot \sqsubset x$

1 and 2 are diferent in information content but equal in information quantity.

Integer is a flat domain, everything except ⊥ is the same information quantity.

$x \sqsubseteq y \to f(x) \sqsubseteq f(y)$ monotone mapping between partially ordered sets.
We cannot use ⊥ as a condition, cannot pattern match on ⊥ or undefined.

$f \sqsubseteq g if \forall x . f(x) \sqsubseteq g(x)$

calculate a sequence of function $f_k$ with the property that each one consists
of the right hand side applied to the previous one

undefined function $f_0(n) = ⊥$

```haskell
g :: (Integer -> Integer) -> (Integer -> Integer)
g x = \n -> if n == 0 then 1 else n * x (n -1)

x0 :: Integer -> Integer
x0 = undefined

(f0:f1:f2:f3:f4:fs) = iterate g x0
```


convergence: directed complete partial order (dcpo) iff every monotone
sequence $x_0 \sqsubseteq x_1 \sqsubseteq \ldots$ has a least upper bound

bottom includes non-termination: $f(n) = f(n+1)$
$f_0 = \bot, f_1 = \bot,\ldots$ a machine executing this program will
loop indefinitely.

Least Fixed Point

--  ⟦ ⟧
a function f is strict iff $f \bot = \bot$

fixed point of a function is an element of the function's domain that is mapped
to itself by the function. c is a fixed point of the function f(x) iff f(c) = c.
f(f(...f(c)...)) = c

f(x) = x^2 - 3x + 4 then 2 is a fixed point of f, because f(2) = 2. Not all functions
have fixed points

Strict Function

A function f is struct if, when applied to a non-terminating expression, it also
fails to terminate. f(⊥) = ⊥. It always evaluates its argument.

Non-strict functions correspond to control structures. It may not evaluate some
of its arguments.

if-then-else is strict in the first parameter and non-strict in the second and
third.

`const _ = 1` is non-strict because it matches on non terminating.

```haskell
const True = 1
const False = 1
```
is strict


~ irrefutable pattern always succeeds, resulting in f(⊥) = 1
```
f ~(Just x) = x + 1
f Nothing   = 2 -- this will never run
```

`let` `where` are non-strict

`let Just x = ...`
`case ... of ~(Just x) -> `

`data [a] = [] | a : [a]`
```haskell
ones :: [Integer]
ones = 1 : ones
```
really should be thought of as potentially infinite list

`⊥ ⊑ 1:⊥ ⊑ 1:1:⊥ ⊑ 1:1:1:⊥ ⊑ ...` infinite list of ones

```
take 0 _      = []
take n (x:xs) = x : take (n-1) xs
take n []     = []
```

```
take 2 ⊥       ==> ⊥
take 2 (1:⊥)   ==> 1 : take 1 ⊥ ==> 1 : ⊥
take 2 (1:1:⊥) ==> 1 : 1 : []
```

strictness annotation

`data Maybe' a = Just' !a | Nothing`

`Just' ⊥ = ⊥`



References:
[Haskell/Denotational Semantics](https://en.wikibooks.org/wiki/Haskell/Denotational_semantics)
[Fixed point (mathematics)](https://en.wikipedia.org/wiki/Fixed_point_(mathematics))
[Strict function](https://en.wikipedia.org/wiki/Strict_function)
