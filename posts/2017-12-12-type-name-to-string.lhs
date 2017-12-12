---
title: Haskell Type Names as Strings
tags: haskell, type-level
---

I will introduce three ways to convert the type, from the type itself or from a value of a type, as a string. Here are all of the language pragmas and imports we will need.

\begin{code}
{-# LANGUAGE DataKinds #-}

-- base
import GHC.TypeLits (symbolVal)
import Data.Proxy (Proxy (..))
import Data.Typeable (typeOf, typeRep)

-- typelits-witnesses
import GHC.TypeLits.List (symbolsVal)
\end{code}

A type declaration we will use later.

\begin{code}
data Person =
  Person
    { name :: String
    , age :: Int
    }
\end{code}

A type without a constructor.

\begin{code}
data X
\end{code}

GHC allows us to use strings as types. Also known as symbols. Symbols are useful when we calculate types and we want to pass some string data along, maybe as a file path or a route (servant does this). It is even possible to perform operations on the string at the type level. We may want to look at what the resulting string is by converting it to a value.

To work with types directly, and not the values of a particular type, we must use `Proxy :: Proxy ...` to capture the type and pass it to a function. This says we are referrencing a type, but not any particular value of a type. A constructor is not required. `symbolVal` converts the type level string to a value level string. If you want to pass a type to a function and run `symbolVal` on int, you must include `KnownSymbol a => ...` as a constraint.

\begin{code}
main :: IO ()
main = do
  print $ symbolVal (Proxy :: Proxy "symbolVal") -- "symbolVal"
\end{code}

Using `symbolsVal`, we can convert a type level list of type level strings to the value level.

\begin{code}
  print $ symbolsVal (Proxy :: Proxy '["first","second"]) -- ["first","second"]
\end{code}

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


=== Notes:

Run with `stack runghc --package typelits-witnesses -- 2017-12-12-type-name-to-string.lhs`.

=== References:

- [Hackage :: base :: Data.Typeable](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Typeable.html)

- [Hackage :: base :: GHC.TypeLits](https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-TypeLits.html)

- [Hackage :: typelits-witnesses](https://hackage.haskell.org/package/typelits-witnesses)
