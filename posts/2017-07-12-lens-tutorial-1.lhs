---
title: Lenses Foundation - SimpleLens
tags: lens,haskell
---

If you have been using Haskell for a while or browsed the packages in Hackage, 
there is a good chance that you have come across a package called `lens`. `lens`
provides a large assorment of types and functions that simplify data access and
updates in a functional way. It can help us solve many problems, but the size 
and scope of the package, as well as complex type signatures, make it 
challenging to approahc for new users. Moreover, we may not need all of the 
tools from the `lens` package and might want to implement a simplified subset of
it. I believe the best way to start using Lenses is Haskell is by implementing 
a simple subset.

The first tool we want to discuss are Lenses. Lenses are functional references. 
Reference means that they point to parts of a value and allow us to access or 
modify them. Functional means that they provide composibility. Lenses abstract
getters and setters for Haskell records (product types).

== Motivation

First, we will review record update syntax in Haskell. You can follow along by
starting up `ghci`. We define a simple data type, make a value of that type and
update one of the rows.

```haskell
λ> data User = User { name :: String, email :: String } deriving (Show)
λ> user = User "Sanjay" "owner@sanjay.com"
λ> updatedUser = user { email = "admin@sanjay.com" }
```

That is relatively simple, but it becomes less manageable when we introduce an
embedded record type.

```haskell
λ> data Phone = Phone { phoneNumber :: String } deriving (Show)
λ> data Employee = Employee { name :: String , phone :: Phone } deriving (Show)
λ> employee = Employee "Guillermo" (Phone "52-33-3333-7400")
λ> updatePhone = (phone employee) { phoneNumber = "52-33-3333-1111" }
λ> updateEmployee = employee { phone = updatePhone }
```


Why do we need Lenses? We will motivate it with a simple example.

```haskell
data Person 
  Person =
    { name :: String
    , age  :: Int
    , address :: String
    }
    
main = do
  let l = Person "Luis" 
  let z = l { address = "" }
  let x = l { address = "" }
```

```haskell
data PhoneType = Mobile | Home | Work

data Phone = 
  Phone 
    { phoneNumber :: String
    , phoneType   :: PhoneType
    }

data Person =
  Person
    { name  :: String
    , age   :: Int
    , phone :: Phone
    }

let fatima = Person "Fatima" 53 (Phone "999111222" Mobile)

let oldPhone  = phone fatima
    newPhone  = phone { phoneNumber ""}
    newFatima = fatima { phone = phoneNumber }
```




```haskell

data Phone 
  Phone 
    { phoneNumber :: String
    } deriving (Eq,Read,Show)

data User
  User 
    { userName     :: String
    , userPassword :: String
    , userPhone    :: Phone
    } deriving (Eq,Read,Show)
    
_phoneNumber :: SimpleLens User String
_phoneNumber a_to_f_a (Phone uPhone) :: SimpleLens User String

_userPhone :: SimpleLens User Phone
_userPhone  a_to_f_a (Phone uPhone) = Phone <$> a_to_f_a uPhone


_name a_to_f_a (Person pName pAge) = (\ppName -> Person ppName pAge) <$> a_to_f_a pName


_userPhone . _phoneNumber
```

\begin{code}
{-# LANGUAGE RankNTypes #-}
\end{code}

implies `ExplicitForAll` forall followed by type parameters. Allows us to use 
`forall` in a type alias.

\begin{code}
import Data.Functor.Const
import Data.Functor.Identity
\end{code}

defines `Const` and `Identity`, two types/type constructors of kind `* -> *`.
Identity is a wrapper, it might seem useless but we will need it.

`b` is a phantom type.

```haskell
newtype Const a b = Const { getConst :: a }

instance Functor (Const m) where
    fmap _ (Const v) = Const v
```

Whatever you try to fmap to Const, the value inside will not change. This is
useful for when you use it in a type that will apply a fmap but you do not 
actually want it to change.

similar to `id`.

```haskell
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap     = coerce

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a
```


\begin{code}
type SimpleLens s a = forall f. Functor f => (a -> f a) -> s -> f s
\end{code}

A `SimpleLens` takes two types. `s` which is a container type with a `* -> *`, 
`* -> * -> *`, like `Maybe`, `[]`, `()`, etc. and the type in the container that 
we want to reference. `SimpleLens Maybe Int`, `SimpleLens Maybe String`,
 `SimpleLens Either Int`, etc.
 
On the right hand side there is a type class restriction for `f`. We will need 
another container `f` that has a Functor instance.

`(a -> f a) -> s -> f s`

The first argument is `(a -> f a)`, we need a function that takes an `a` and 
returns it in the `f` container. `f` has a Functor instance and contain some 
element `a`. `a` is the type we are referencing that is contained by `s`, then 
we pass it an instance of `s` and we get `s` in `f`. We need a function from.

- SimpleLens is a type synonym.
- SimpleLens has two type parameters: `s` the container type and `a` the type 
of the record in the container.
- SimpleLens requires a second container type `f` that has a `Functor` instance.
- `(a -> f a)` and `s` and it returns `f s`, the container type wrapped in the 
second container.


\begin{code}
data Person =
  Person
    { name :: String
    , age  :: Int
    } deriving (Eq,Read,Show)


_name :: SimpleLens Person String
_name a_to_f_a (Person pName pAge) = (\ppName -> Person ppName pAge) <$> a_to_f_a pName

_age :: SimpleLens Person Int
_age a_to_f_a (Person pName pAge) = (\ppAge -> Person pName ppAge) <$> a_to_f_a pAge
\end{code}

by fmapping Person the `s` type into `f a`, `_name` returns a type `f s` because


`_name` is a lens that focuses on the `name` record of `Person`. `a_to_f_a` is 
the function we need to pass in `(a -> f a)` and we apply it directly to the 
name record of Person. Note that the type of name matches `SimpleLens s a` is
`SimpleLens Person String`.

This means we still need to a function that has a functor instance to `_name` when 
we try to apply it to a `Person` value.

\begin{code}
view :: SimpleLens s a -> s -> a
-- view :: ((a -> f a) -> s -> f s) -> s -> a
view l = getConst . l Const
\end{code}

`view` takes `_name` and provides `_name` with `Const` for its `(a -> f a)` 
function. 

view lifts the 
`newtype Const a b = Const { getConst :: a }`

```haskell
instance Functor (Const m) where
    fmap _ (Const v) = Const v
```

throw away whatever gets passed to Const in a functor. Useful for when a fmap 
is expected as part of a function but we do not want any changes.

`view` passes `Const` which has kind `* -> * -> *` to the lens we pass to `view`.
`getConst` returns the first value and ignores the second.

\begin{code}
set :: SimpleLens s a -> a -> s -> s
set l b = runIdentity . l (\_ -> Identity b)
\end{code}

\begin{code}
over :: SimpleLens s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)

--over :: ASetter s t a b -> (a -> b) -> s -> t
--over l f = runIdentity #. l (Identity #. f)
\end{code}


\begin{code}
data Phone =
  Phone 
    { phoneNumber :: String
    } deriving (Eq,Read,Show)

data User =
  User 
    { userName     :: String
    , userPassword :: String
    , userPhone    :: Phone
    } deriving (Eq,Read,Show)
    
_phoneNumber :: SimpleLens User Phone
_phoneNumber a_to_f_a (User uName uPassword uPhone) = (\newPhone -> User uName uPassword uPhone) <$> a_to_f_a uPhone

_userPhone :: SimpleLens Phone String
_userPhone  a_to_f_a (Phone phoneNum) = (\z -> Phone z) <$> a_to_f_a phoneNum

--blah = User "Gupta" "asdfasdfasdf" (Phone "123-456-789")
--blah2 = view (_)
--_name a_to_f_a (Person pName pAge) = (\ppName -> Person ppName pAge) <$> a_to_f_a pName


--_userPhone . _phoneNumber
\end{code}

\begin{code}
sanjay = Person "Sanjay" 23
sanjayName = view _name sanjay
jay = set _name "Jay" sanjay
jayName = view _name jay

sanjayOlder = over _age (+1) sanjay

main = do 
  print sanjayName
  print sanjayOlder
\end{code}



```
over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity #. l (Identity #. f)
```

a lens focuses on a single target

== Lens laws

- [24 Days of GHC Extensions: Rank N Types](https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html)

- [GHC Users Guide :: 9.20. Arbitrary-rank polymorphism](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#arbitrary-rank-polymorphism)

- [hackage :: base :: Data.Functor.Const](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Functor.Const.html#Const)

- [hackage :: base :: Data.Functor.Identity](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Functor.Identity.html#Identity)
