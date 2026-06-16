---
title: Linear Types in Haskell
kind: tutorial
state: complete
tags: haskell, linear-types
---

I heard about Linear Types a long time. Maybe about ten years ago I explored a system called [Liquid Haskell](https://github.com/ucsd-progsys/liquidhaskell). It's a Haskell compiler that adds refinement types (use predicate logic in the type system to further restrict the range of values that a type can hold). I had considered it for a production system, but at the time it felt like too much overhead. That started my exploration into type theory and related tools.

Linear types say that a value must be used exactly one time in the body of a function. Support for linear types was added to [GHC 9.0 in 2020](https://discourse.haskell.org/t/glasgow-haskell-compiler-9-0-1-rc1-now-available/1706). In GHC Haskell, we need to use the  [LinearTypes language extension](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html) and to make life easier, the [linear-base library](https://hackage.haskell.org/package/linear-base-0.8.0#readme).

In the type signature of a function, we append `%1` after the type, to tell the compiler we will only use the type once in the body of the function.

```haskell
f :: a %1 -> b
```

Linear types makes us think about how values are used in a function. A perfect domain for linear types is money. Imagine there an authority that can create and destroy money (non-linear), then there are a set of accounts that can hold money that authority. The accounts can transfer money (linear, they can only use it once) but they cannot create or destroy it. Let's get started.

\begin{code}
-- the language extension
{-# LANGUAGE LinearTypes #-}

-- import some tools from linear base
import Data.Unrestricted.Linear (Ur (Ur), move)
\end{code}

`Ur` is an unrestricted value. In the context of linear types, it lets us know that the value can be used as many times as we like. `move :: a %1 -> Ur a ` takes a linear value and makes it unrestricted. For me the main idea here, is that when we see `Ur` or `move`, we need to think carefully about what we are doing with value and make sure it does not violate the restrictions we have for the linear types within the system we define.

Now we will define a few types.

\begin{code}
type Amount = Integer

newtype Money = Money Amount -- do not export the constructor

-- inject money into a system
mint :: Amount -> Money
mint = Money
\end{code}

With `mint`, we have a way to create money. We also need a way to remove money from a system, so with `burn` we take the amount of money as a linear type, and return it as an `Ur Amount` so it acts like a record of how much was removed.

\begin{code}
-- remove money from a system
burn :: Money %1 -> Ur Amount
burn (Money a) = move a
\end{code}

Now we create some accounts that can receive and transfer money.

\begin{code}
type AccountId = Integer

data Account = Account AccountId Money

openAccount :: AccountId -> Amount -> Account
openAccount i a = Account i (mint a)

closeAccount :: Account %1 -> Ur Amount
closeAccount (Account i bal) = case move i of
  Ur _ -> burn bal
\end{code}

The most interesting function here is `closeAccount`. It takes an `Account` as a linear type because when we close it, we expect it to be gone, so it cannot be used afterwards. The `Amount` returned is intended to be a record of the amount of money that was removed from the system.

Now we will go over a few helper functions for money.

\begin{code}
amount :: Money %1 -> (Ur Amount, Money)
amount (Money a) = case move a of
  Ur x -> (Ur x, Money x)

split :: Amount -> Money %1 -> Either Money (Money, Money)
split n (Money a) = case move a of
  Ur x
    | n <= x -> Right (Money n, Money (x - n))
    | otherwise -> Left (Money x)

merge :: Money %1 -> Money %1 -> Money
merge (Money a) (Money b) = case move a of
  Ur x -> case move b of
    Ur y -> Money (x + y)
\end{code}

`amount` reads the amount of money without destroying it.

`split` takes an amount of money such that it conserves the total amount of Money that was put in. If the request amount was too much, then it returns `Left` preserving the original amount. If it was less than or equal, it returns `Right` with the first value as the requested amount and the second value as the original amount minus the second amount.

`merge` takes two money values and combines them.

Now the last three `Account` functions before we make a test program.

\begin{code}
balanceOf :: Account %1 -> (Ur Amount, Account)
balanceOf (Account i bal) = case move i of
  Ur i' -> case amount bal of
    (amt, bal') -> (amt, Account i' bal')
\end{code}

`balanceOf` takes an account, returns its amount for inspection and preserves the values in the account.

\begin{code}
deposit :: Money %1 -> Account %1 -> Account
deposit m (Account i bal) = case move i of
  Ur i' -> Account i' (merge m bal)
\end{code}

`deposit` increments the amount held in an account using the `merge` function.

\begin{code}
transfer :: Amount -> Account %1 -> Account %1 -> (Account, Account, Ur Bool)
transfer n (Account si sbal) dst = case move si of
  Ur si' -> case split n sbal of
    Left whole -> (Account si' whole, dst, Ur False)
    Right (moved, rest) -> (Account si' rest, deposit moved dst, Ur True)
\end{code}

`transfer` tries to move an amount of money from one account to another. It uses the `split` function. If it fails, then the accounts are unchanged. If it succeeds, then the first account is updated for the reduced amount and the second account receives a `deposit`. The `Bool` value tracks if `transfer` was succesful of not.

\begin{code}
main :: IO ()
main = do
  let a = openAccount 1 100
      b = openAccount 2 200 
      c = openAccount 3 300
      (a1, b1, Ur _transferResult1) = transfer 500 a b 
      (b2, c2, Ur _transferResult2) = transfer 150 b1 c
      (a3, c3, Ur _transferResult3) = transfer 10 a1 c2
      (Ur a4Amount, a4) = balanceOf a3
      (Ur b4Amount, b4) = balanceOf b2
      (Ur c4Amount, c4) = balanceOf c3

  putStrLn ("Account A holds: " <> show a4Amount)
  putStrLn ("Account B holds: " <> show b4Amount)
  putStrLn ("Account C holds: " <> show c4Amount)
  
  let _a = closeAccount a4
      _b = closeAccount b4
      _c = closeAccount c4

  pure ()
\end{code}

Hopefully that gives you a basic idea of how to use Linear Types in Haskell. Here are some exercises I have designed if you want further practice.

1. Make a Bank type that tracks the total amount of money in the accounts (sum of all the balances of the Accounts). The bank can create and destroy money.

2. Create multiple banks that track the total amount of money in accounts associated with them. Track the total amount each bank has in their associated accounts and the total all banks have. Allow accounts to transfer between each other in the same bank. Create a wire function to allow transferring between accounts in different banks and the banks collect a fee.

3. Create an Economy type and a Currency type. The economy type is responsible for creating and removing money from the system. Allow it to give and collect money from banks. Banks still maintain the same relation with accounts as described in number two. Each economy has their own currency. Banks and accounts can transfer or wire money within the same economy. They can also perform international transfers to different economies, but there is an exchange rate and they need to report foreign transfers to the corresponding economy.
