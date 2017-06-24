---
title: An Introduction to Type Families
---

## Review

Before discussing type families, we need to review type synonyms, type classes, 
multi-parameter type classes and functional dependencies.

#### Type synonyms

Type synonyms are a form of syntactic sugar. They allow us to give a new name 
to an existing type. You can treat them as the same as the original type. They 
are useful annotations for the programmer, but they can be ignored by the 
compiler.

```haskell
type Name = String

λ> let x = "Sanjay" :: Name
λ> let y = "Sanjay" :: String
λ> x == y
True
```

#### Type classes

Type classes allow us to put constraints on polymorphic type parameters. For 
example, the `print` function from `base` puts a constraint on the its 
polymorphic type variable.

```haskell
class Show a where
  show :: a -> String

print :: Show a => a -> IO ()
```

We can read it is as `print` accepts any type `a` that has an instance of the 
`Show` type class. The compiler will reject any `a` that does not have an 
instance of `Show`. When we make an instance of a type class for a type, we 
overload the operations associated with that particular type class. In this 
case `Show` is expecting a type of kind `*`.

Type classes can also support higher-kinded types such as `* -> *`.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

While `* -> *` is not explicitly stated, the `f` occurs on the left hand side of
a type variable, the compiler can infer that `f` is a type of kind `* -> *`. 

In Haskell, type class instances are global, import any file with a type class 
instance and that instance is automatically propagated to all other files 
importing it. Then, for any function requiring a type class, the instance are 
implicitly used. One type cannot have multiple instances of a single type class.

#### Multi-parameter type classes

By default, Haskell 2010 does not support multi-parameter type classes. We need
to add the language pragma `{-# LANGUAGE MultiParamTypeClasses #-}` to enable 
them. 

```haskell
class X a b where
```

We will often need `{-# LANGUAGE FlexibleInstances #-}` to handle Multi-parameter 
type classes when we want to add more restrictions to instances.

```haskell
class Show [Int] where
class X (Maybe Int) String where
```

Type classes are a set of types. Multi-parameter type classes is a relationship 
between types. 

#### Functional dependencies

Because multi-parameter type classes may introduce ambiguity. We need 
`{-# LANGUAGE FunctionalDependencies #-}` to constrain the parameters of type 
classes.

```haskell
class Combine a b c | a b -> c where
  combine :: a -> b -> c
```

The right hand side of the class declaration `| a b -> c` states that `c`'s type 
is determined by `a` and `b`. For any given `a` and `b` you can only have one `c`.
`c` should not be a free variable, it is determined by the other two variables.

Here is general container type class using functional dependencies.

```haskell
class Container e c | c -> e where
  empty  :: c
  insert :: e -> c -> c
  member :: e -> c -> Bool
  toList :: c -> [e]

instance Eq e => Container e [e] where
  empty           = []
  insert e l      = (e:l)
  member e []     = False
  member e (x:xs) 
    | e == x      = True
    | otherwise   = member e xs
  toList l        = l
```








## Type Families

With type classes, we can associate the set of functions from a type class with 
a type. Type families allow us to associate types with a type. They define 
partial functions from types to types by pattern matching on input types. 
Type families are type-indexed data types and name functions on types. The data 
types and functions are retrieved by the types provided. 

Type families come in three variations: 

- data families
- open type synonym families
- closed type synonym families

## Data Family

Data family type families create a new data type. They restrict the codomain and 
require each instance to define a new type constructor for the function's result. 

They can be declared at the top level, with or without kind declaration. 

```haskell
data family Array a
data family Array :: * -> *
```

Or they can be declared within a class. These are known as associated types.

```haskell
class GeneralMapKey k where
  data GeneralMap k :: * -> *
```

An array example with an associated type.

```haskell
class ArrayElem e where
  data Array e
  index :: Array e -> Int -> e

instance ArrayElem Bool where
    data Array Bool = BoolArray BitVector
    index (BoolArray ar) i = indexBitVector ar i

instance ArrayElem Int where
    data Array Int = IntArray UIntArr
    index (IntArray ar) i = indexUIntArr ar i

instance (ArrayElem a, ArrayElem b) => ArrayElem (a, b) where
    data Array (a, b) = PairArray (Array a) (Array b)
    index (PairArray ar br) = (index ar i, index br i)
```

## Type Synonym Families

Type synonym families permit types in the codomain of the type function to be 
any type with the appropriate kind. 

Open type families are declared at the top level.

```haskell
type family F a :: *
```

We can close a type synonym family by adding a `where` clause. Also, it 
guarantees that the associated functions will be tried in order of declaration.

```haskell
type family F a where
  F Int  = Double
  F Bool = Char
  F a    = String
```

#### Examples

A general concat type family can be used to concat different types. You must 
decide what the resulting type is for any pair of types.

```haskell
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text (Text)

class Concat a b where
  type ConcatTy a b -- the declared type will replace this type synonym
  cat :: a -> b -> ConcatTy a b

instance Concat Text String where
  type ConcatTy Text String = Text
  cat x y = x <> (T.pack y)

instance Concat String Text where
  type ConcatTy String Text = Text
  cat x y = (T.pack x) <> y

instance Concat String ByteString where
  type ConcatTy String ByteString = String
  cat x y = x ++ (BS.unpack y)
  
main = print $ cat ("Hello" :: Text) (" World!" :: String)
```

A general container type family.

```haskell
class Container c where
  type Elem c
  empty  :: c
  insert :: Elem c -> c -> c
  member :: Elem c -> c -> Bool
  toList :: c -> [Elem c]
  
instance Eq e => Container [e] where
  type Elem [e]   = e
  empty           = []
  insert e l      = (e:l)
  member e []     = False
  member e (x:xs) 
    | e == x      = True
    | otherwise   = member e xs
  toList l        = l

instance Eq e => Container (Maybe e) where
  type Elem (Maybe e) = e -- type synonym
  empty            = Nothing
  insert e l       = Just e -- destructive, replaces previous element
  member e Nothing = False
  member e (Just x) = e == x
  toList Nothing   = []
  toList (Just x)  = [x]
```

## What to Remember About Type Families

- Data families create new types, every instance of a data family declares new constructors.

- Type families can only refer to other existing types.

## References

- [Haskell Wiki :: GHC/Type families](https://wiki.haskell.org/GHC/Type_families)

- [GHC Users Guide :: 10.9. Type families](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#type-families)

- [StackOverflow](https://stackoverflow.com/a/20908500/412417)

- [Wikipedia :: Type family](https://en.wikipedia.org/wiki/Type_family)
