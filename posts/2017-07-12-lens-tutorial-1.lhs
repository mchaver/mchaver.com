---
title: Lens Tutorial - SimpleLens
tags: haskell, lens
---

If you have been using Haskell for a while or browsed the packages in Hackage, 
there is a good chance that you have come across a package called `lens`. `lens`
provides a large assorment of types and functions that simplify data access and
updates in a functional way. It can help us solve many problems, but the size 
and scope of the package, as well as complex type signatures, make it 
challenging for new users to approach. Moreover, we may not need all of the 
tools from the `lens` package. I believe the best way to start using Lenses is 
Haskell is by implementing a simple subset.

The first tool we want to discuss are Lenses. Lenses are functional references. 
Reference means that they point to parts of a value and allow us to access or 
modify them. Functional means that they provide composibility. Lenses abstract
getters and setters for Haskell product types (records).

== Motivation

First, we will review record update syntax in Haskell. You can follow along by
starting up `ghci`. We define a simple data type, make a value of that type and
update one of the rows.

```haskell
λ> data User = User { name :: String, email :: String } deriving (Show)
λ> user = User "Sanjay" "owner@sanjay.com"
λ> updatedUser = user { email = "admin@sanjay.com" }
```

That is relatively simple, but it becomes more complex when we introduce an
embedded record type.

```haskell
λ> data Phone = Phone { phoneNumber :: String } deriving (Show)
λ> data Employee = Employee { name :: String , phone :: Phone } deriving (Show)
λ> employee = Employee "Guillermo" (Phone "52-33-3333-7400")
λ> updatePhone = (phone employee) { phoneNumber = "52-33-3333-1111" }
λ> updateEmployee = employee { phone = updatePhone }
```

Lenses can help simplify this problem.

== Background

There are two types from `base` that we need to understand before we begin with
Lenses: `Identity` and `Const`. We will focus on how they work. Once we get to 
Lenses we understand their purpose.

==== Data.Functor.Identity

You may have come across the `id` function before. It takes a value and returns 
it. `id` is useful for when you are required to provide a function but do not 
want to change the value.

```haskell
λ> id "Hello world!"
"Hello world!"
λ> id (1 + 1)
2
```

`Identity` is similar `id`, but it is a newtype that has one type parameter and 
has an instance of `Functor`. `Identity` is a container type like `Maybe`. 

```haskell
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap = coerce
```

We can make the `Functor` instance for `Identity` a bit clearer.

```haskell
instance Functor Identity where
  fmap f (Identity i) = Identity $ f i
```

Try out `Identity` in `ghci`.

```haskell
λ> (++ " world!") <$> Identity "Hello"
Identity "Hello world!"
λ> runIdentity $ (+1) <$> Identity 1
2
```

==== Data.Functor.Const

`const` is another common function. It takes two items, returns the first and
discards the second.

```haskell
λ> const True "Hello world!"
True
λ> const 1 2
1
λ> const "Hello world!" Nothing
"Hello world!"
```

Much like the relation between `id` and `Identifier`, there are `const` and 
`Const`. `Const` has two type parameters `a` and `b`, but it only takes and 
returns a value of type `a`. `b` is a phantom type. `b` does not exist on the 
right side of the declaration and we do not provide a value of type `b`. 

The `Functor` instance for `Const` is also 
interesting. It ignores the function and does not 
apply it to value of type `a`. The returned valued remains constant.

```haskell
newtype Const a b = Const { getConst :: a }

instance Functor (Const m) where
  fmap _ (Const v) = Const v
```

We can try out `Const` in `ghci`.

```haskell
λ> not <$> Const True
Const True
λ> getConst $ (+1) <$> Const 1
1
λ> (++ " world!") <$> Const "Hello"
Const "Hello"
```

== SimpleLens

We will implment a simplified version of Lens called SimpleLens.

\begin{code}
{-# LANGUAGE RankNTypes #-}

import Data.Functor.Const
import Data.Functor.Identity
\end{code}

`RankNTypes` implies `ExplicitForAll`. It allows us to use `forall` in a type 
alias. We import `Const` and `Identity` as we discussed above.

\begin{code}
type SimpleLens s a = forall f. Functor f => (a -> f a) -> s -> f s
\end{code}

A `SimpleLens` has two polymorphic types. `s` is a container type like 
`Maybe`, `[]`, `(,)`, `Either`, etc. `a` is the type in the container that 
we want to reference. For example, `SimpleLens Maybe Int`, 
`SimpleLens (,) String`, `SimpleLens Either Int`, etc.
 
On the right hand side there is a type class restriction for `f`. We will need 
another container type `f` that has a Functor instance. This is where `Const`
and `Identity` will be used.

The first argument is `(a -> f a)`, this a function that takes an `a` and 
returns `a` in the `f` container, which has a Functor instance. `a` is the type 
we are referencing that is contained by `s`, then we pass it an instance of `s` 
and we get `s` contained in `f`.

Now we define our first lens. Person will be the `s` type and `String` for 
`name` and `Int` for `age` will be the `a` types in each `SimpleLens`.

\begin{code}
data Person =
  Person
    { name :: String
    , age  :: Int
    } deriving (Eq,Read,Show)

-- expanded type signature
-- _name :: forall f. Functor f => (String -> f String) -> Person -> f Person
_name :: SimpleLens Person String
_name a_to_f_a (Person pName pAge) = (\ppName -> Person ppName pAge) <$> a_to_f_a pName

-- expanded type signature
-- _age :: forall f. Functor f => (Int -> f Int) -> Person -> f Person
_age :: SimpleLens Person Int
_age a_to_f_a (Person pName pAge) = (\ppAge -> Person pName ppAge) <$> a_to_f_a pAge
\end{code}

`_name` is a lens that focuses on the `name` record of `Person`. `a_to_f_a` is 
the function we need to pass in `(a -> f a)` and we apply it directly to the 
name record of Person. `_age` is a lens that focuses on the `age` record of 
Person.

By themselves we cannot do anything directly with `_name` or `_age`. We will 
need some helper functions. Before continuing, here is what we should now about
`SimpleLens` so far:

- `SimpleLens` is a type synonym.

- `SimpleLens` has two polymorphic type parameters: `s` is a container type, `a` 
the type of a value contained in `s`.

- It takes `(a -> f a)` and `s` and it returns `f s`. The container type `s` 
wrapped in a second container `f`.

- `f` has a `Functor` instance.

== SimpleLens helper functions

==== view

The first helper function we will implement is `view`. `view` takes a 
`SimpleLens` and an `s` then it returns an `a` from `s`. `view` functions as a
getter. It does not change the value we are referencing, it just returns it. We
will use `Const` to retrieve `a` from `s`.

\begin{code}
-- view :: ((a -> f a) -> s -> f s) -> s -> a
view :: SimpleLens s a -> s -> a
view l = getConst . l Const
\end{code}

We can use `view` and `_name` together to get the `name` record from `Person`.

```haskell
λ> view _name $ Person "Marina" 21
"Marina"
```

The way `view` and `_name` work together may still be a bit unclear. It is 
useful to write out what `view name` would look like.

```haskell
-- view _name
view_name :: Person -> String
view_name (Person pName pAge) = getConst $ (\ppName -> Person ppName pAge) <$> Const pName
```

If you remember the definition of `Functor Const` the `f` function will not 
applied to the value inside `Const`. `(\ppName -> Person ppName pAge)` will be
ignored and `getConst $ Const pName` will be reduced to `pName`.

To solidify our understanding of `view` and `SimpleLens`, we will repeat the 
same for age.

```haskell
-- view _age
view_age :: Person -> Int
view_age (Person pName pAge) = getConst $ (\ppAge -> Person pName pAge) <$> Const pAge
```

==== set

`set` is the setter helper function for `SimpleLens`. It takes a `SimpleLens`,
an `a` that we want to insert in `s`, `s` and it returns `s` with the new `a`
value. `set` uses `Identity` to apply the new `a` into `s` and return `s`.

\begin{code}
-- set :: ((a -> f a) -> s -> f s) -> a -> s -> s
set :: SimpleLens s a -> a -> s -> s
set l b = runIdentity . l (\_ -> Identity b)
\end{code}

Here is an example.

```haskell
λ> set _name "Serena" $ Person "Marina" 21
Person "Serena" 21
```

Just like we did above, we explicit write out `set` with `_name` to make sure 
we understand how `set` works with a lens.

```haskell
-- set _name
set_name :: String -> Person -> Person
set_name b (Person pName pAge) = runIdentity $ (\ppName -> Person ppName pAge) <$> Identity b
```

==== over

`over` is the same as `set` except instead of taking a value of `a`, it takes a
function `(a -> a)`. It allows us to modify an existing value inside `s`

\begin{code}
-- over :: ((a -> f a) -> s -> f s) -> (a -> a) -> s -> s
over :: SimpleLens s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f)
\end{code}

And a simple example.

```haskell
λ> over _age (+1) $ Person "Marina" 21
Person "Marina" 22
```

Expanded form of `over _age`.

```haskell
-- over _age
over_age :: (Int -> Int) -> Person -> Person
over_age a_to_a (Person pName pAge) = runIdentity $ (\ppAge -> Person pName ppAge) <$> Identity (a_to_a pAge)
```

==== SimpleLens with embedded record

\begin{code}
data Phone = 
  Phone 
    { phoneNumber :: String 
    } deriving (Show)

data Employee = 
  Employee 
    { employeeName :: String
    , employeePhone :: Phone 
    } deriving (Show)
    
_phoneNumber :: SimpleLens Phone String
_phoneNumber a_to_f_a (Phone phoneNum) = (\pPhoneNum -> Phone pPhoneNum) <$> a_to_f_a phoneNum

_employeePhone :: SimpleLens Employee Phone
_employeePhone a_to_f_a (Employee eName ePhone) = (\eEPhone -> Employee eName eEPhone) <$> a_to_f_a ePhone

_employeeName :: SimpleLens Employee String
_employeeName a_to_f_a (Employee eName ePhone) = (\eEName -> Employee eEName ePhone) <$> a_to_f_a eName

main :: IO ()
main = do 
  let matthias         = Employee "Matthias" $ Phone "123-345-8888"
      matthiasNewPhone = set (_employeePhone . _phoneNumber) "222-333-1212" matthias
      matthiasJr       = set (_employeePhone . _phoneNumber) "432-234-1177" $ over _employeeName (++ " Jr.") $ matthias
  print matthias
  print matthiasNewPhone
  print matthiasJr
\end{code}

Here is clean implementation of [SimpleLens.hs](https://gist.github.com/mchaver/d00a4ea654c618cf16457d9de84f1a02)
that you can use to play around with.

== References

- [GHC Users Guide :: 9.20. Arbitrary-rank polymorphism](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#arbitrary-rank-polymorphism)

- [hackage :: base :: Data.Functor.Const](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Functor.Const.html#Const)

- [hackage :: base :: Data.Functor.Identity](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/Data.Functor.Identity.html#Identity)
