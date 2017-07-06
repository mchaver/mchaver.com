---
title: SKI Combinator Calculus in JavaScript
---

Combinatory logic is a variant of lambda calculus that does not have bound 
variables. SKI combinator calculus is a combinatory logic, a reduced version of 
untyped lambda calculus. While it is impractical for real world use, it is 
an extremely simple Turing complete language. Lambda calculus can be translated 
into SKI calculus as binary trees. Combinatory logic eliminates free variables. 
A combinator is a higher-order function that uses only function application 
to define a result.

## Combinatory terms

- `x` variable
- `P` primitive function
- `(E₁E₂)` application of combinatory terms

## Primitive Combinators

It requires only two primitive functions: 

- `K x y = x` constance function
- `S x y z = x z (y z)` substitution function

but commonly there is a third primitive function for convenience.

- `I x = x` identity function

`I` is the same as `SKK`.

## Examples

Identify without the primitive identity function.

```
((S K K) x)
    = (S K K x)
    = (K x (K x))
    = x
```

Constant function.

```
(K (K a b) (K a))
    = (K (a) (K a))
    = a
```

Limited reduction.

```
(K a)
    = (K a)
```

Boolean logic. True is `T` and returns the first argument.

```
Txy = Kxy = x
```

False is `F` and returns the second argument.

```
SKxy = Ky(xy) = y
```

## JavaScript Implementation

There is a `cons` function to create LISP style cons lists. The `mkExpression`
function creates an associative array with a `type`, `primitive` or `variable` 
and a `value` which is a single character.

#### stringToTree

Remove all white space because there may be multiple white spaces between 
parentheses and single character primitives and variables. Then prepend 
everything with a white space to be able to split everything into an array and 
use `fold` to create a cons binary tree.

#### fold

Recursively build a cons binary tree from an array.

#### fixedPoint

Recursively `reduce` a tree until `reduce` does not make any changes to the 
tree. 

#### reduce

Perform `convert` on a subtree if the node has a primitive on the left hand side.

#### convert

If the `leftDepth` matches the number of arguments that a primitive takes and 
a primitive exists at that left side depth, perform a reduction.

#### leftDepth

Count the number of `car`s in a tree.

## References

- [Wikipedia :: Combinatory logic](https://en.wikipedia.org/wiki/Combinatory_logic)
- [Wikipedia :: SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
- [Wikipedia :: Unlambda](https://en.wikipedia.org/wiki/Unlambda)
