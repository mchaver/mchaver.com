---
title: Chapter 18 - Recursive Types
date:  2016-07-17
---

$\mu$: explicit recursion operator for types.

NatList = \mu X . <nil : Unit, cons: {Nat,X}>
Let NatList be the infinite type satisfying the equation X = <nil:Unit, cons:{Nat,X}>

equi-recursive
iso-recursive

differ amount of help that the programmer is expected to give the typechecker.

hungry function: can accept any number of numeric arguments and always return
a new function that is hungry for more.

$Hungry \; = \; \mu A . Nat \to A;$

define an element of this type with the fix operator

$f = fix (\lambda f : Nat \to Hungry. \lambda n : Nat . \; f);$

Stream function can consume an arbitrary number of unit values, each time returning
a pair of a number and a new stream.

$Stream \; = \; \mu A . Unit \to {Nat, A}$

and two stream destructors, s is a stream, hd s is the first number it returns when
we pass it unit.

$hd = \lambda s : Stream. (s unit). 1$
hd : Stream \to Nat

tl s is the new stream that we obtain when we pass unti to s.

$tl = \lambda s : Stream. (s unit). 2;$
tl : Stream \to Stream

use fix to construct a stream

$upFrom0 = fix (\lambda f : Nat \to Stream. \lambda n : Nat. \lambda _ : Unit. {n, f (succ n)}) 0;$
upFrom0 : Stream

hd upFrom0;
0 : Nat

hd (tl (tl (tl upfrom0)));
3 : Nat

Streams can be generalized to processes, functions that accept a number and return
a number and a new process.

$Process = \mu A. Nat \to {Nat,A}$

\\[
p = fix (\lambda f : Nat \to Process. \lambda acc : Nat . \\lambda : Nat \lambda : Nat. \\
  let newacc = plus acc n in
  {newacc, f newacc}) 0;
\\]


$curr = \lambda s : Process. (s 0). 1$
$sned = \lambda n : Nat . \lambda s : Process. (s n). 2;$

$curr (send 20 (send 3 (send 5 p)));$



###Formalities
NatList and <nil:Unit, cons:{Nat:NatList}>
equi-recursive: treats these two as definitionally equal, interchangeable in all
context, since they stand for the same infinite tree. typechecker is responsible
for making sure that a term of one type will be allowed as an argument to a function
expecting the other.

iso-recursive: treats a recurisive type and its unfolding as different but isomorphic.
THe unfolding of a recursive type $\mu X.T$ is the type obtained by taking the
body T and replacing all occurrences of X by the whole recursive type $[X \mapsto (\mu X.T)]T$

$\mu X . <nil:Unit, cons:{Nat,X}$ unfolds to $<nil:Unit, cons:{Nat, \mu X. <nil:Unit, cons:{Nat,X}>}>$

introduce two functions for to map back and forth

$unfold[\mu X . T] : \mu X.t \to [X \mapsto \mu X . T]T$
$fold[\mu X . T] : [X \maptso \mu X . T]T \to \mu X . T$
