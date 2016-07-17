---
title: Laziness
date:  2016-07-12
---

####Non-strictness vs. Laziness
Non-strict semantics: nothing will be evaluated until it is needed.
Lazy evaluation: how you implement non-strictness using thunks.

thunks: unevalutated values with a recipe of how to evaluate them.

Weak head normal form

empty list is in normal form (cannot be evaluated further).
at an intermediate step it is in weak head normal form.
Fully evaluating WHNF reduces it to normal form. Evaluation on a value is called
forcing the value.

some functions evaluate more than others
`length [*thunk*,*thunk*]` whnf, then normal form
vs.
`length $ show x` normal form

a function is strict if passing undefined will result in an error. If it were a
a lazy function, passing it undefined would print no error.
`head (4 : undefined)`
f is strict iff `f undefined` results in an error and a halt of the program.

f ⊥ = ⊥ ⇔ f is strict

lazy pattern `~()` should only be use if you have a single constructor for a type like
a tuple ()

separation of concerns without time penalty
improved code usage

[Laziness](https://en.wikibooks.org/wiki/Haskell/Laziness)
[The Point of Laziness](https://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/)
[More Points for Lazy Evaluation](https://augustss.blogspot.tw/2011/05/more-points-for-lazy-evaluation-in.html)
[Lazy evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation)
