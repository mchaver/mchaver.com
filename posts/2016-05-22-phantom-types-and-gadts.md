---
title: Phantom Types and Generalized Algebraic Data Types
tags: haskell
---

## Phantom Types

A phantom type is a parameterized type in the left hand side type constructor
which does not get used in the in the right hand side data constructor. The 
type has no values associated with it.

```haskell
data Length a = Length Double
```

`Length` is a phantom type because `a` does not appear on the right hand side. 
It helps track a type.

```haskell
data Inch
data Centimeter

inchesToCentimeters :: Length Inch -> Length Centimeter
inchesToCentimeters (Length in) = Distance (2.54 * in)

centimetersToInches :: Length Centimeter -> Length Inch
centimetersToInches (Length cm) = Length (cm / 2.54)
```

## Data Kinds

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

data LengthUnit = Inch | Centimeter

-- restrict the type of a polymorphic variable
newtype Length (a :: LengthUnit) = Distance Double
  deriving (Num, Show)

inchesToCentimeters :: Length 'Inch -> Length 'Centimeter
inchesToCentimeters (Length in) = Distance (2.54 * in)

centimetersToInches :: Length 'Centimeter -> Length 'Inch
centimetersToInches (Length cm) = Length (cm / 2.54)
```

## Generalized Algebraic Data Types

weak approximations of inductive families from dependently typed languages

A `data` constructor allows us to define a type constructor on the left hand 
side and a data (value) constructor on the right hand side. Generalized 
Algebraic Data Types (GADTs) allow us to explicitly write down the type 
signatures of data constructors. In GADTs, the return type of the data 
constructor does not have to match the type constructor.

We can expand on our example from above:

```haskell
{-# LANGUAGE GADTs #-}

data Inch
data Centimeter

data Length a where
  InchLength       :: Double -> Distance Inch
  CentimeterLength :: Double -> Distance Centimeter

deriving instance Show (Distance a)

inchesToCentimeters :: Length Inch -> Length Centimeter
inchesToCentimeters (Length in) = Distance (2.54 * in)

centimetersToInches :: Length Centimeter -> Length Inch
centimetersToInches (Length cm) = Length (cm / 2.54)
```

Here is another motivating example from the GHC Users Guide:

```haskell
data Term a where
  Lit    :: Int -> Term Int
  Succ   :: Term Int -> Term Int
  IsZero :: Term Int -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a
  Pair   :: Term a -> Term b -> Term (a,b)  
```

We can then pattern match on all of the constructors of `Term`.

```haskell
eval :: Term a -> a
eval (Lit i)      = i
eval (Succ t)     = 1 + eval t
eval (IsZero t)   = eval t == 0
eval (If b e1 e2) = if eval b then eval e1 else eval e2
eval (Pair e1 e2) = (eval e1, eval e2)
```

GADTs allow type refinement. Type `a` is refined to `Int` in the case of the 
`Lit` constructor because of the signature in the GADT definition 
`Lit :: Int -> Term Int`. 

```haskell
eval :: Term a -> a
eval (Lit i)      = i
```

## References

- [GHC Users Guide :: GADTs](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#ghc-flag--XGADTs)

- [StackOverflow :: phadej :: Motivation behind Phantom Types?](https://stackoverflow.com/a/28250226/412417)

- [Wikibooks :: Haskell/GADT](https://en.wikibooks.org/wiki/Haskell/GADT)
