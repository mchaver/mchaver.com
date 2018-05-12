---
title: Introduction to GHC.Generics
tags: haskell, generics
---

Generic functions traverse the structure of a type to produce the body of a function with the goal of reducing code reptition.

Imagine a simple record type and a function that converts any type to a `String`.

```haskell
data Person
  = MkPerson
  { name :: String
  , age :: Int
  }
  
genericShow :: a -> String
genericShow = undefined
```

Here is a simplified version of how we traverse over the structure of the data type to produce the body of the `genericShow` function. The following is pseudo code and does not actually compile. It intends to emulate the way GHC.Generics works in a simple, readable manner.

```haskell
genericShow :: a -> String
genericShow a =
  case getTypeRepresentation a of
    TypeName name rest = show name ++ genericShow rest

    Constructor constructor rest = show constructor ++ genericShow rest

    NamedField (Just fieldName) typ = show fieldName ++ show type
    
    UnnamedField Nothing type = show typ
    
    ConcatFields field1 field2 = genericShow field1 ++ genericShow field2
    
    ConcatConstructors constructor1 constructor2 = genericShow constructor1 ++ genericShow constructor2
    
    TypeParameter a = genericShow a
	
    NoFields = ""
```

## Examples of Generic Representations

#### Product (Record with Fields)

```haskell
data Person
  = MkPerson
  { name :: String
  , age :: Int
  }

TypeName "Person"
  (Constructor "MkPerson"
    (NamedField (Just "name") "String" 
	`ConcatFields`
	 NamedField (Just "age") "Int"))
```

#### Sum Type without Values

```haskell
data Empty

TypeName "Empty"
  (Constructor "Empty" NoFields)
```

#### Sum Type with One Value

```haskell
data Wrapper = Wrapper Int

TypeName "Wrapper"
  (Constructor "Wrapper" (NamedField Nothing "Int"))
```

#### Sum Type with Multiple Constructors and Type Parameters

```haskell
data Either a b
  = Left a
  | Right b
  
TypeName "Either"
  ((Constructor "Left" (TypeParameter a))
   `ConcatConstructors`
   (Constructor "Right" (TypeParameter b)))
```

#### GHC.Generic Types

We can view use our simple representation as type synonyms for the real types in GHC.Generics.

```haskell
type TypeName = D1 ('MetaSelector name moduleName packageName isNewType) rest

type Constructor = C1 ('MetaCons constructor prefixOrInfix isRecord) rest

type NamedField = S1 ('MetaSel ('Just selector) unpackednesss sourceStrictness decidedStrictness)

type UnnamedField = S1 ('MetaSel Nothing unpackednesss sourceStrictness decidedStrictness)

type ConcatFields = (:*:)

type ConcatConstructors = (:+:)

type TypeParameter a = Rec0 a

type NoFields = U1

```

## GHC.Generic Representations

Now we will take a look at the real generic repsentations of the types from above. 
In GHCi, you can see the representations by calling `:kind! Rep` on any type that
derives `Generic`

#### Product (Record with Fields)

```haskell
:set -XDeriveGeneric
import GHC.Generics (Generic, Rep)

data Person
  = MkPerson
  { name :: String
  , age :: Int
  } deriving Generic

:kind! Rep Person

Rep Person :: * -> *
= D1
    ('MetaData "Person" "Ghci2" "interactive" 'False)
    (C1
       ('MetaCons "MkPerson" 'PrefixI 'True)
       (S1
          ('MetaSel
             ('Just "name")
             'NoSourceUnpackedness
             'NoSourceStrictness
             'DecidedLazy)
          (Rec0 String)
        :*: S1
          ('MetaSel
		      ('Just "age")
              'NoSourceUnpackedness
			  'NoSourceStrictness
			  'DecidedLazy)
		  (Rec0 Int)))
```

#### Sum Type without Values

```haskell
Rep Empty :: * -> *
= D1
    ('MetaData "Empty" "Ghci3" "interactive" 'False)
    V1
```

#### Sum Type with One Value

```haskell
data Wrapper = Wrapper Int deriving Generic

Rep Wrapper :: * -> *
= D1
    ('MetaData "Wrapper" "Ghci4" "interactive" 'False)
    (C1
       ('MetaCons "Wrapper" 'PrefixI 'False)
       (S1
          ('MetaSel
             'Nothing
             'NoSourceUnpackedness
             'NoSourceStrictness
             'DecidedLazy)
          (Rec0 Int)))
```

#### Sum Type with Multiple Constructors and Type Parameters

```haskell
data Either a b
  = Left a
  | Right b
  deriving Generic


:kind! Rep (Either Int String)

Rep (Either Int String) :: * -> *
= D1
    ('MetaData "Either" "Ghci5" "interactive" 'False)
    (C1
       ('MetaCons "Left" 'PrefixI 'False)
       (S1
          ('MetaSel
             'Nothing
             'NoSourceUnpackedness
             'NoSourceStrictness
             'DecidedLazy)
          (Rec0 Int))
     :+: C1
       ('MetaCons "Right" 'PrefixI 'False)
       (S1
          ('MetaSel
             'Nothing
             'NoSourceUnpackedness
             'NoSourceStrictness
             'DecidedLazy)
          (Rec0 [Char])))
```

You can also see the generic representations when you build the package: `stack build --ghc-options=-ddump-deriv`.
