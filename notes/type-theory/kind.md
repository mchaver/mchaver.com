Kind is the type of a type constructor or rather the type of a higher-order type
operator. A kind system is a simple typed lambda calculus with a primitive type
written a * and called type, which is the kind of any data type which does not
need any type parameters.

Kind: type of a data type, arity specifier.
polymorphic types are type constructors
non-polymorphic types are nullary type constructors, monomorphic types also have
the same kind *.

$*$ pronounced type
$* \to *$ unary type constructor (list type, maybe type)
$* \to * \to *$ binary type constructor (pair type, function type (arrow) constructor ) $\cdot \to \cdot$
$(* \to *) \to *$ is a higher-order type operator from unary type constructor to proper type.

`Int` kind is `*`
`Maybe` kind is `* -> *` it requires one type

##Kinds in Haskell

 - $*$ is the kind of all data types.
 - $k_1 \to k_2$ is the kind of a unary type constructor, which takes a type of
 kind $k_1$ and produces a type of kind $k_2$.

 `4 :: Int`, `[1,2,3] :: [Int]` all have kind `*`, so do `Int -> Bool` and `Int -> Int -> Bool`.

 A type constructor takes one or more arguments and produces a data type when enough
 are supplied. `[]` type constructor has type `* -> *`. `(,)` has `* -> * -> *`
 and `(,,)` has `* -> * -> * -> *`

 ghci
```
:k Maybe
Maybe :: * -> *
```

* is the kind of lifted types, represent s the kind of types that can contain
runtime values.
# is the kind of unlifted types

unlifted data types are algebraic data types which live in the kind `#` rather 
than kind `*`.

 Primitive (unboxed) types cannot be defined in Haskell, and are therefore built
 into the language and compiler. Primitive types are always unlifted; that is,
 a value of a primitive type cannot be bottom.

[Kind (type theory)](https://en.wikipedia.org/wiki/Kind_(type_theory))
[Type constructor](https://en.wikipedia.org/wiki/Type_constructor)
[Function type](https://en.wikipedia.org/wiki/Function_type)
[What exactly is the kind "*" in Haskell?](https://stackoverflow.com/questions/27095011/what-exactly-is-the-kind-in-haskell)
[(GHC) The Kind of (->) seems magic](https://www.reddit.com/r/haskell/comments/2u6dne/ghc_the_kind_of_seems_magic/co5poc5)
[Kind](https://wiki.haskell.org/Kind)
[Unlifted data types](https://ghc.haskell.org/trac/ghc/wiki/UnliftedDataTypes)
[7.2. Unboxed types and primitive operations](https://downloads.haskell.org/~ghc/6.12.3/docs/html/users_guide/primitives.html)
