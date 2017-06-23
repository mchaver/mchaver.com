---
title: Injective Type Families
---

Allow functional dependency annotations on type families. This allows one to 
define injective type families.

resolve type ambiguities in situations where a type variable appears only under type family applications.


a partial function from X to Y (written as f: X ↛ Y) is a function f: X ′ → Y, for some subset X ′ of X. It generalizes the concept of a function f: X → Y by not forcing f to map every element of X to an element of Y (only some subset X ′ of X). If X ′ = X, then f is called a total function and is equivalent to a function.

An injective function is a one-to-one function that preserves distinction. It 
never maps distinct elements of its domain to the same element of its codomain. 

A bijective function uniquely maps all elements in both domain and codomain to 
each other.

a function f from a set X to a set Y is surjective (or onto), or a surjection, 
if for every element y in the codomain Y of f there is at least one element x 
in the domain X of f such that f(x) = y. It is not required that x is unique;

A non-surjective function.

- Definition (Generativity). If f and g are generative, then f a ~ g b implies f ~ g

- Definition (Injectivity). If f is injective, then f a ~ f b implies a ~ b

- Definition (Matchability). A function f is matchable iff it is generative and injective


https://stackoverflow.com/questions/39707115/data-families-vs-injective-type-families

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#injective-type-families
