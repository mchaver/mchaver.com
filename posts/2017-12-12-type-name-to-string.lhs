---
title: Haskell Type Names as Strings
tags: haskell, type-level
---

I will introduce a few different ways to convert a type, from the type itself or from a value of a type, to a string. Here are all of the language pragmas and imports we will need.

\begin{code}
{-# LANGUAGE DataKinds #-} -- support Symbol and type level lists
{-# LANGUAGE DeriveGeneric #-} -- derive Generic for Person
{-# LANGUAGE TypeFamilies #-} -- define type level operations
{-# LANGUAGE UndecidableInstances #-} -- allow `TypeName (Rep a ())` below

-- base
import GHC.Generics
import GHC.TypeLits (symbolVal, Symbol)
import Data.Proxy (Proxy (..))
import Data.Typeable
  ( typeOf
  , typeRep
  , typeRepTyCon
  , tyConPackage
  , tyConModule
  , tyConName
  )

-- typelits-witnesses
import GHC.TypeLits.List (symbolsVal)
\end{code}

A type declaration we will use later.

\begin{code}
data Person =
  Person
    { name :: String
    , age :: Int
    } deriving (Generic)

type Human = Person
\end{code}

A type without a constructor.

\begin{code}
data X
\end{code}

\ignore{
\begin{code}
type family TypeName a :: Symbol where
  TypeName Double = "Double"
  TypeName Int = "Int"
  TypeName String = "String"

  TypeName (M1 D ('MetaData name _ _ _) f ()) = name
  TypeName a = TypeName (Rep a ()) -- requires UndecidableInstances
\end{code}
}


GHC allows us to use strings as types. Also known as `Symbol`s. `Symbol`s are useful when we calculate types and we want to pass some string data along, maybe as a file path or a route (servant does this), to another type. It is even possible to perform operations on the string at the type level. We may want to look at what the resulting string is by converting it to a value.

To work with types directly, and not the values of a particular type, we must use `Proxy :: Proxy ...` to capture the type and pass it to a function. This says we are referrencing a type, but not any particular value of a type. A constructor is not required. `symbolVal` converts the type level string to a value level string. If you want to pass a type to a function and run `symbolVal` on int, you must include `KnownSymbol a => ...` as a constraint.

== Runtime : Symbol to String

\begin{code}
main :: IO ()
main = do
  print $ symbolVal (Proxy :: Proxy "symbolVal") -- ("symbolVal" :: Symbol) to ("symbolVal" :: String)
\end{code}

Using `symbolsVal`, we can convert a type level list of type level strings to the value level.

\begin{code}
  print $ symbolsVal (Proxy :: Proxy '["first","second"]) -- ["first","second"]
\end{code}

== Runtime : Type Proxy to String

Get the type name of non-symbols. If you want to pass a type to a function and run `typeRep` on in, you must include `TypeRep a => ...` as a constraint.

\begin{code}
  print $ typeRep (Proxy :: Proxy Int) -- "Int"
  print $ typeRep (Proxy :: Proxy Person) -- "Person"
  print $ typeRep (Proxy :: Proxy X) -- "X"
\end{code}

Get the type name of a value.

\begin{code}
  print $ typeOf "hello" -- "[Char]"
  print $ typeOf $ Person "Susana" 40 -- "Person"
\end{code}

`typeRep` returns `TypeRep` for a `Proxy` of a type and `typeOf` returns `TypeRep` for a value of a type. `show` on a `TypeRef`. `typeRepTyCon` turns a `TypeRep` into a `TyCon` and allows us to extract more useful information.

\begin{code}
  let pTyCon = typeRepTyCon $ typeRep (Proxy :: Proxy Person)
\end{code}

Package name where Person is defined.

\begin{code}
  print $ tyConPackage pTyCon -- "main"
\end{code}

Module name where Person is defined.

\begin{code}
  print $ tyConModule pTyCon -- "Main"
\end{code}

Type level constructor.

\begin{code}
  print $ tyConName pTyCon --  "Person"
  print $ tyConName $ typeRepTyCon $ typeRep (Proxy :: Proxy Maybe) -- "Maybe" (it is not "Just" or "Nothing", those are value level constructors
\end{code}


== Compile Time : Type to Symbol

Finally, there may be a case where you want to convert a type into `Symbol` while you are still compiling. For example, for a given type build a servant route base on the `Symbol` of the type. GHC does not provide a way to automatically convert a type to `Symbol` so we need to define our own type family to handle this using `GHC.Generics`. For any type that does not have a `Generic` instance, you must manually define what its `Symbol` is.

```haskell
type family TypeName a :: Symbol where
  TypeName Double = "Double"
  TypeName Int = "Int"
  TypeName String = "String"

  TypeName (M1 D ('MetaData name _ _ _) f ()) = name
  TypeName a = TypeName (Rep a ()) -- requires UndecidableInstances
```

Now we can convert a type to a `Symbol` and the `Symbol` to a `String`.

\begin{code}
  print $ symbolVal (Proxy :: Proxy (TypeName Int)) -- convert Int to ("Int" :: Symbol) to ("Int" :: String)
  print $ symbolVal (Proxy :: Proxy (TypeName Person)) -- convert Person to ("Person" :: Symbol), using Generic, then to ("Int" :: String)
  print $ symbolVal (Proxy :: Proxy (TypeName String))
  print $ symbolVal (Proxy :: Proxy (TypeName Human))
\end{code}

=== Notes:

Run with `stack runghc --package typelits-witnesses -- 2017-12-12-type-name-to-string.lhs`.

=== References:

- [Hackage :: base :: Data.Typeable](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Typeable.html)

- [Hackage :: base :: GHC.Generics](https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-Generics.html)

- [Hackage :: base :: GHC.TypeLits](https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-TypeLits.html)

- [Hackage :: typelits-witnesses](https://hackage.haskell.org/package/typelits-witnesses)
