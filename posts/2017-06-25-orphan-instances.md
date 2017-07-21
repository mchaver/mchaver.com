---
title: Orphan Instances
tags: haskell
---

## Global type class instances

In Haskell, type class instances are global and unique. For any type, it can 
only have one instance of a type class and it is implicitly imported into 
anything that imports a file with the instance or a file that imports a file 
with an instance.

For example, we declare a data type `Person` with an instance for `Show`.

```haskell
module A where
  
data Person =
  Person 
    { name :: String
    , age  :: Int
    }
    
instance Show Person where
  show (Person n a) = 
    "This person is named " ++ name ++ " and is " ++ (show age) ++ " years old."
```

In another module, we import `A`. This includes the type class instances that 
`Person` has implemented. We cannot create a new instance of `Show Person`. 
The compiler will complain if we try to make a new instance of `Show Person`.

```haskell  
module B where
import A
```

In a third module we only import `B`, but do not import `A`. Again, we 
cannot create a new instance of `Show Person` because it has been implicitly 
imported because `B` imports `A`.

```haskell  
module C where
import B
```

## Orphan instance

Assume that `A` belongs to a library package called `person`. Now we make a new 
executable package called `website` that depends on `person` and we import the 
`Person` type, but we realize we forgot to add an instance of `Eq` so we decide 
to declare it in the website package.

```haskell
module Website where
import A

instance Eq Person where
  (==) (Person _aName aAge) (Person _bName bAge) = aAge == bAge
```

This works, but the compiler will complain that we have declared an orphan 
instance. Orphan instances can cause problems if we declare an instance for a
type in two different files and then a third file imports both of these files.
That would bring in two instances of the same typeclass/type pair and that is 
not permissible in Haskell. We want to avoid orphan instances as much as 
possible.

There are two common workarounds:

- Define all the type class instances where the type is defined. This is easy if 
you own the code or you are willing to maintain a branch of the library.

- Create a newtype of the type we want more type classes for. This will avoid 
orphan instances, but require a lot more code and may be annoying if there are a
lot of type specific functions we want to use.

## Are orphan instances ever acceptable?

If you are declaring type instances in an executable package or a test package 
that is not shared, declaring orphan instances generally is not a problem because
nothing else will import those instances and it will not contaminate other 
libraries. 

We can silence the compiler with a simple annotation at the top any file with 
orphan instances.

```haskell
{-# OPTIONS_GHC -fno-warn-orphans #-}
```

If you have any private libraries, it might be ok to declare orphan instances 
as well.

## References

- [Haskell wiki :: Orphan instance](https://wiki.haskell.org/Orphan_instance)
