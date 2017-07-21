---
title: Kinds in Haskell
tags: haskell
---

## Constructors

- type constructor arguments are types
- data constructor arguments are values of the specified types

The keyword `data` declares a type constructor on the left hand side and a data 
constructor on the right hand side. `Bool` is a simple example. None of the 
constructors take arguments.

```haskell
data Bool = True | False
```

`Bool` is the type constructor and `True` and `False` are the data constructors.

```haskell
data User = User
  { name :: String
  , age  :: Int
  }
```

There is a type constructor `User` that takes no arguments and a data 
constructor `User` that takes a `String` and an `Int`.

```haskell
data Maybe a = Nothing | Just a
```

`Maybe` is a type constructor that takes a single argument, `Nothing` is a data
constructor that takes no arguments and `Just` is a data constructor that takes 
one argument of type `a`. The type constructor of `Just 1` would be `Maybe Int`.
The type for a data constructor maybe polymorphic (determined by the the type 
constructor) or fixed (using a type like `Int`, `Maybe String`, `(Int,String)`, 
etc.).

## Kind

A kind is the type of a type constructor. Ordinary types have the kind `*`. This 
means they do not need another type as an argument and can be directly related
to a value. For example, `Int` can have values such as `0`, `1`, `2`, and 
`[Int]` can be `[0,1,2]`. These types are inhabited types. They have values. 

A type like `[]` or `Maybe` has the kind `* -> *`. It needs to be given a type 
before it can be related to any values. These types are unihabited types. They
do not have values. When we provide a type to these types, we can make inhabited
types like `[Int]`, `[String]`, `Just Double` and they have values like `[0,1,2]`,
`["hello","goodbye"]` and `[1.5,2.5,3.5]`.

In `ghci` we can query the kind of a type with `:k`. `:k Int` returns
`Int :: *`, `:k Maybe` returns `Maybe :: * -> *`. Here are the kinds of some
common types.

```haskell
-- Nullary type constructors
Int :: *
String :: *

-- Unary type constructors
[] :: * -> *
Maybe :: * -> *
Vector :: * -> *

-- Unary type constructors applied to one type
[Int] :: *
Maybe String :: *
Vector Int :: *
```

There are more complicated kinds. We can even have partially applied kinds. 
`Either`, `(,)` and `(->)` all require two kinds `* -> * -> *`. If we give any
of these only one type `Either String` or `(,) Int` then the kind is `* -> *`.

```haskell
-- Binary type constructors
Either :: * -> * -> *
(,) :: * -> * -> *
(->) :: * -> * -> *

-- Binary type constructors applied to one type
Either Int :: * -> *
(,) Int :: * -> *
(->) Int :: * -> *

-- Binary type constructors applied to two types
Either Int String :: *
(,) Int String :: *
(Int,String) :: *
(->) Int String :: *
Int -> String :: *
```

#### What to Remember about Kinds

- Kinds are the type of a type constructor.
- Kinds represent the arity (number of parameters) of a type constructor.
- Kinds are a higher-order type operator.
- `*` is the kind of an inhabited type. Inhabited types have values.
- All other kinds (`* -> *`, `* -> * -> *`, `(* -> *) -> *`, etc.) are uninhabited types. Uninhabited types do not have values.

#### Names of Kinds

- `*` nullary type constructor.
- `* -> *` unary type constructor.
- `* -> * -> *` binary type constructor.
- `(* -> *) -> *` higher-order type constructor.


## GHC Language Extensions

#### DataKinds

Allows promotion of data types to kind level. Promote datatype to be a kind 
and its data constructor to be a type constructor. Promoted constructors are
prefixed with `'`.

```haskell
λ> data Nat = Zero | Succ Nat

λ> :k 'Zero
'Zero :: Nat

λ> :k 'Succ
'Succ :: Nat -> Nat

λ> data List a = Nil | Cons a (List a)

λ> :k Nil
'Nil :: List a

λ> :k Cons
'Cons :: a -> List a -> List a
```

#### PolyKinds

Allows kind polymorphic types. This introduces kind variables. We can name a 
variable that has a particular kind signature in the type constructor and then 
apply it to a type in the data constructor.

Assume we want to have a constructor for an `Int` that is contained in something
but we do not care what the container is.

```haskell
λ> data A (x :: * -> *) = B (x Int)

λ> :k A Maybe
A Maybe :: *

λ> :k A []
A [] :: *

λ> let x = B $ Just 1
λ> let y = B [1,2,3]

λ> :t x
y :: A Maybe

λ> :t y
y :: A []
```

`Data.Proxy` uses a `*` in the type constructor and ignores it in the data 
constructor.

```haskell
λ> data KProxy (x :: *) = KProxy

λ> :k KProxy Int
KProxy Int :: *

λ> :k KProxy (Int,String)
KProxy (Int,String) :: *
```

#### TypeOperators

Allows the use and definition of of types with operator names (symbols) like 
`(+)`, `(:>)`, `:<|>`, `.:`, etc.

```haskell
λ> data a + b = Plus a b

λ> :t Plus 1 2
Plus 1 2 :: (Num b, Num a) => a + b

λ> :t Plus 1 "123"
Plus 1 "123" :: (Data.String.IsString b, Num a) => a + b

λ> type Foo = Int + Bool

λ> :k Foo
Foo :: *
```

#### TypeInType

Allows kind declaration and operations to be as descriptive as types: explicit
quantification over kind variables, higher-rank kinds, type synonyms and 
families in kinds, etc.

## References

- [GHC Users Guide :: DataKinds](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#ghc-flag--XDataKinds)

- [GHC Users Guide :: PolyKinds](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#ghc-flag--XPolyKinds)

- [GHC Users Guide :: TypeInType](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#ghc-flag--XTypeInType)

- [GHC Users Guide :: TypeOperators](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#ghc-flag--XTypeOperators)

- [Haskell Wiki :: Constructor](https://wiki.haskell.org/Constructor)

- [Haskell Wiki :: Kind](https://wiki.haskell.org/Kind)
