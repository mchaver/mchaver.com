---
title: Type-Level Heterogeneous List
tags: haskell, type-level
---

HList is a well designed package for type level heterogeneous lists. We will 
take a look at the main declaration to get more familiar with type level 
programmin in Haskell.

Promote data types to to kind level. We want to use `[]` at the type level to 
operate over a list of types that we use to calculate the type signature.

\begin{code}
{-# LANGUAGE DataKinds          #-}
\end{code}

Allow multiparameter typeclass instances `instance C (Either a String) where ...`.

\begin{code}
{-# LANGUAGE FlexibleContexts   #-}
\end{code}

Allow typeclass instances for nested types `instance C (Maybe Int) where ...`.

\begin{code}
{-# LANGUAGE FlexibleInstances  #-}
\end{code}

Allow `deriving instance` separated from the type declaration. It can be in the 
same file or another file.

\begin{code}
{-# LANGUAGE StandaloneDeriving #-}
\end{code}

Allow indexed and data type families. The instances of data families can be data 
types and newtypes.

\begin{code}
{-# LANGUAGE TypeFamilies       #-}
\end{code}

Allow the use and definition of types with operator names. 

\begin{code}
{-# LANGUAGE TypeOperators      #-}
\end{code}

`HList` is a list of inhabited types. They must be of kind `*`. `HNil` is an 
empty list of types. `HCons` is a constructor to concatentate to types.

\begin{code}
data family HList (l :: [*])
data instance HList '[] = HNil
data instance HList (x ': xs) = x `HCons` HList xs
\end{code}

With `deriving instance`, `Eq` and `Ord` are straightforward.

\begin{code}
deriving instance Eq (HList '[])
deriving instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving instance Ord (HList '[])
deriving instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs))
\end{code}

Finally, we can iterate over the types to create an instance of `Show` and run 
a simple example.

\begin{code}
instance Show (HList '[]) where
  show _ = "H[]"

instance (Show e, Show (HList l)) => Show (HList (e ': l)) where
  show (HCons x l) = 
    let 'H':'[':s = show l
    in "H[" ++ show x ++
         (if s == "]" then s else "," ++ s)
         
infixr 2 `HCons`

foo :: HList '[Char, String, Double]
foo = 'Z' `HCons` "Zebra" `HCons` 2.5 `HCons` HNil

main :: IO ()
main = print foo
\end{code}

== References

- [Hackage :: HList](https://hackage.haskell.org/package/HList)

- [Strongly Typed Heterogeneous Collections](http://okmij.org/ftp/Haskell/HList-ext.pdf)
