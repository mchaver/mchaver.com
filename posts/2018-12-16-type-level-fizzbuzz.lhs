---
title: A Preliminary Attempt at Type-Level FizzBuzz
tags: haskell, type-level
---

[FizzBuzz](http://wiki.c2.com/?FizzBuzzTest) is a test to assess basic programming knowledge.
Having done a bit of type-level programming GHC Haskell, I thought it would be a fun exercise to write
FizzBuzz at the type-level.

We will use `Symbol`s, type-level strings that support string literals, and `Nat`s, type-level natural numbers that support unsigned integer literals.

== The Language Extensions

We need a number of language extensions enabled because type-level programming is not a part of the Haskell standard.

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
\end{code}

Kinds are the types of types, DataKinds allows types defined with `data` to be used at the type-level. We need this to do operations on `Nat` and `Symbol`. PolyKinds allows support for type-level functions that take more than one parameter.


\begin{code}
{-# LANGUAGE TypeOperators #-}
\end{code}

Use operators like `+` at the type-level.

\begin{code}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
\end{code}

TypeFamilies allows us to define type-level functions, a function that takes a type and returns a type, and we need UndecidableInstances to call a type-level function within a type-level function.

\begin{code}
import Data.Proxy (Proxy(..))
import GHC.TypeLits (CmpNat, Mod, symbolVal, KnownSymbol)
\end{code}

- `Proxy` passes a type as a value.
- `CmpNat` compares natural numbers at the type-level, returns Ordering (EQ, LT, GT).
- `symbolVal` is a function that converts a `Symbol`, a type-level string, into a `String`.

\begin{code}
type family IfThenElse cond a b where
  IfThenElse 'True a _ = a
  IfThenElse _     _ b = b
\end{code}

You will quickly find that there are very few basic functions available for type-level programming.
There is not even an if-else control structure at the type-level. `'True` is a `Bool` constructor promoted the type-level.

\begin{code}
type family NatToSym x where
  NatToSym 0 = "0"
  NatToSym 1 = "1"
  NatToSym 2 = "2"
  NatToSym 3 = "3"
  -- to 100
\end{code}

Neither is there a function to convert `Nat` to `Symbol` so we need to write them by hand. For FizzBuzz we only need 1-100. I did this quickly with `seq 0 100 | awk '{printf("  NatToSym %s = \"%s\"\n", $1, $1)}'`.

\ignore{
\begin{code}
  NatToSym 4 = "4"
  NatToSym 5 = "5"
  NatToSym 6 = "6"
  NatToSym 7 = "7"
  NatToSym 8 = "8"
  NatToSym 9 = "9"
  NatToSym 10 = "10"
  NatToSym 11 = "11"
  NatToSym 12 = "12"
  NatToSym 13 = "13"
  NatToSym 14 = "14"
  NatToSym 15 = "15"
  NatToSym 16 = "16"
  NatToSym 17 = "17"
  NatToSym 18 = "18"
  NatToSym 19 = "19"
  NatToSym 20 = "20"
  NatToSym 21 = "21"
  NatToSym 22 = "22"
  NatToSym 23 = "23"
  NatToSym 24 = "24"
  NatToSym 25 = "25"
  NatToSym 26 = "26"
  NatToSym 27 = "27"
  NatToSym 28 = "28"
  NatToSym 29 = "29"
  NatToSym 30 = "30"
  NatToSym 31 = "31"
  NatToSym 32 = "32"
  NatToSym 33 = "33"
  NatToSym 34 = "34"
  NatToSym 35 = "35"
  NatToSym 36 = "36"
  NatToSym 37 = "37"
  NatToSym 38 = "38"
  NatToSym 39 = "39"
  NatToSym 40 = "40"
  NatToSym 41 = "41"
  NatToSym 42 = "42"
  NatToSym 43 = "43"
  NatToSym 44 = "44"
  NatToSym 45 = "45"
  NatToSym 46 = "46"
  NatToSym 47 = "47"
  NatToSym 48 = "48"
  NatToSym 49 = "49"
  NatToSym 50 = "50"
  NatToSym 51 = "51"
  NatToSym 52 = "52"
  NatToSym 53 = "53"
  NatToSym 54 = "54"
  NatToSym 55 = "55"
  NatToSym 56 = "56"
  NatToSym 57 = "57"
  NatToSym 58 = "58"
  NatToSym 59 = "59"
  NatToSym 60 = "60"
  NatToSym 61 = "61"
  NatToSym 62 = "62"
  NatToSym 63 = "63"
  NatToSym 64 = "64"
  NatToSym 65 = "65"
  NatToSym 66 = "66"
  NatToSym 67 = "67"
  NatToSym 68 = "68"
  NatToSym 69 = "69"
  NatToSym 70 = "70"
  NatToSym 71 = "71"
  NatToSym 72 = "72"
  NatToSym 73 = "73"
  NatToSym 74 = "74"
  NatToSym 75 = "75"
  NatToSym 76 = "76"
  NatToSym 77 = "77"
  NatToSym 78 = "78"
  NatToSym 79 = "79"
  NatToSym 80 = "80"
  NatToSym 81 = "81"
  NatToSym 82 = "82"
  NatToSym 83 = "83"
  NatToSym 84 = "84"
  NatToSym 85 = "85"
  NatToSym 86 = "86"
  NatToSym 87 = "87"
  NatToSym 88 = "88"
  NatToSym 89 = "89"
  NatToSym 90 = "90"
  NatToSym 91 = "91"
  NatToSym 92 = "92"
  NatToSym 93 = "93"
  NatToSym 94 = "94"
  NatToSym 95 = "95"
  NatToSym 96 = "96"
  NatToSym 97 = "97"
  NatToSym 98 = "98"
  NatToSym 99 = "99"
  NatToSym 100 = "100"
  NatToSym  _ = ""
\end{code}
}

\begin{code}
type family OrderToBool x where
  OrderToBool 'EQ = 'True
  OrderToBool _   = 'False
\end{code}

`CmpNat` returns `Ordering`, convert `'EQ` to ''True' and `'GT` and `'LT` to `'False`.

\begin{code}
type family ModRemainderIsZero x y where
  ModRemainderIsZero x y = OrderToBool (CmpNat (Mod x y) 0)
\end{code}

This checks if the remainder of `Mod` is zero or not.

\begin{code}
type family FizzBuzz x where
  FizzBuzz x =
    IfThenElse (ModRemainderIsZero x 5)
      (IfThenElse (ModRemainderIsZero x 3) "FizzBuzz" "Buzz")
      (IfThenElse (ModRemainderIsZero x 3) "Fizz" (NatToSym x))
\end{code}

At the type-level we do not have guards or let-binding, but it should be pretty straightforward. Compare this to FizzBuzz in a value-level Haskell function.

\begin{code}
fizzbuzz :: Int -> String
fizzbuzz x
  | modRemainderIsZero 15 = "FizzBuzz"
  | modRemainderIsZero 3 = "Fizz"
  | modRemainderIsZero 5 = "Buzz"
  | otherwise = show x
  where
    modRemainderIsZero = (0 ==) . mod x
\end{code}

It would be nice to be able to write a `Map` function and apply it to a type-level list of `Nat`s, but a type-level function (closed type family) cannot
be curried. My other idea was to apply `(\x -> maybe (pure ()) (\y -> print $ symbolVal (Proxy :: Proxy (FizzBuzz y))) (someNatVal x))` on `[0..100]`, but
`someNatVal` converts a `Integer` to `SomeNat`, which does not allow us to extract the `Nat` value and convert it to a `Symobl`. It seems like GHC needs to know the `Nat` type at compile
time. This led me to write out each line with `seq 0 100 | awk '{printf("  printSymbolVal (Proxy :: Proxy (FizzBuzz %s))\n", $1)}'`

\begin{code}
printSymbolVal :: (KnownSymbol a) => Proxy a -> IO ()
printSymbolVal proxy = print $ symbolVal proxy

main :: IO ()
main = do
  printSymbolVal (Proxy :: Proxy (FizzBuzz 1))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 2))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 3))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 4))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 5))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 6))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 7))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 8))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 9))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 10))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 11))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 12))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 13))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 14))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 15))
\end{code}

The original goal was to solve it in a more succint way, create a range of `Nat`s at the type-level, apply `FizzBuzz` to each `Nat` and concatenate the result into a single `Symbol`, but the `Map` part was not feasible do to the lack of currying support for closed type families.

\ignore{
\begin{code}
  printSymbolVal (Proxy :: Proxy (FizzBuzz 16))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 17))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 18))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 19))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 20))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 21))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 22))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 23))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 24))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 25))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 26))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 27))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 28))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 29))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 30))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 31))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 32))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 33))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 34))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 35))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 36))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 37))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 38))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 39))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 40))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 41))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 42))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 43))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 44))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 45))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 46))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 47))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 48))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 49))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 50))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 51))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 52))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 53))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 54))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 55))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 56))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 57))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 58))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 59))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 60))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 61))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 62))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 63))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 64))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 65))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 66))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 67))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 68))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 69))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 70))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 71))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 72))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 73))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 74))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 75))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 76))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 77))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 78))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 79))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 80))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 81))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 82))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 83))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 84))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 85))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 86))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 87))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 88))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 89))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 90))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 91))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 92))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 93))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 94))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 95))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 96))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 97))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 98))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 99))
  printSymbolVal (Proxy :: Proxy (FizzBuzz 100))
\end{code}
}
