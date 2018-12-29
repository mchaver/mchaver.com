---
title: Improved Type-Level FizzBuzz
tags: haskell, type-level
---

This is a slight improvment of the code in my previous post [A Preliminary Attempt at Type-Level FizzBuzz](/posts/2018-12-16-type-level-fizzbuzz.html). The credit goes to my friend [Shulhi Sapli](https://shulhi.com/). He took a look at my code and came up with a cleaner solution. This part should be straightforward if you were able to folow the code from the previous post.

\begin{code}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits

type family IsZero (a :: Nat) :: Bool where
  IsZero 0 = 'True
  IsZero _ = 'False

type family ModRemainderIsZero (a :: Nat) (b :: Nat) where
  ModRemainderIsZero a b = IsZero (Mod a b)

type family FizzBuzz' (a :: Bool) (b :: Bool) c where
  FizzBuzz' 'True 'True _ = "FizzBuzz"
  FizzBuzz' _     'True _ = "Fizz"
  FizzBuzz' 'True _     _ = "Buzz"
  FizzBuzz' _     _     c = NatToSym c
\end{code}

`ConcatSymbols` is a type-level function to concat each `Symbol` with a line break in between each value. That way we can transform a list of `Nat` with `NatToSym` and `ConcatSymbols` into a single `Symbol`, turn it into a `String` and print it.

\begin{code}
type family ConcatSymbols xs where
  ConcatSymbols '[] = ""
  ConcatSymbols (x ': xs) = AppendSymbol x (AppendSymbol "\n" (ConcatSymbols xs))
\end{code}

The nicest part is he found a solution to type-level function mapping. He got the idea from (Thinking with Types Type-Level Programming in Haskell)[http://thinkingwithtypes.com/] in chapter 10. This type-level technique is called [defunctionalization](https://en.wikipedia.org/wiki/Defunctionalization), which allows for higher order type-level functions in Haskell.

\begin{code}
type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

data FizzBuzz :: Nat -> Exp Symbol
type instance Eval (FizzBuzz n) = FizzBuzz' (ModRemainderIsZero n 3) (ModRemainderIsZero n 5) n
\end{code}

Now we just need a list of `Nat`s and we can map `FizzBuzz` and `ConcatSymbols` to get a `Symbol`. Much cleaner than the previous version.

\begin{code}
type Nums = '[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]

type Result = ConcatSymbols (Eval (MapList FizzBuzz Nums))

main :: IO ()
main = putStr $ symbolVal (Proxy :: Proxy Result)
\end{code}

One thing I have not been able to do is create a working `Range` type-level function that takes two `Nat` and returns all the numbers between them in a list. The following compiles.

\begin{code}
type family IfThenElse cond a b where
  IfThenElse 'True  a _ = a
  IfThenElse 'False _ b = b

type family Append xs y where
  Append '[] y = '[y]
  Append (x ': xs) y = x ': (Append xs y)

type family OrderToBool x where
  OrderToBool 'EQ = 'True
  OrderToBool _   = 'False

type family Range (x :: Nat) (y :: Nat) (zs :: [Nat]) :: [Nat] where
  Range x y zs =
    IfThenElse
      (OrderToBool (CmpNat x y))
      zs
      (Range (x + 1) y (Append zs x))
\end{code}

I want to compile and run the following, but it seems to get caught an infinite loop. The compiler suggest using the GHC flag `-freduction-depth=0`, but it does not seem to help. Even for small values it gets stuck.

```haskell
type Result2 = ConcatSymbols (Eval (MapList FizzBuzz (Range 1 3 '[])))

main :: IO ()
main = putStr $ symbolVal (Proxy :: Proxy Result2)
```


\ignore{
\begin{code}
type family NatToSym x where
  NatToSym 0 = "0"
  NatToSym 1 = "1"
  NatToSym 2 = "2"
  NatToSym 3 = "3"
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
