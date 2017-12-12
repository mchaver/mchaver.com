---
title: Haskell Introduction to Functors
---

## Prerequisites

- `data` type declarations and data constructors

- function composition

- type classes: `class` and `instance`

- polymorphic parametricity 

## Simple Example

You should be already familiar with the `Maybe` data type. 

```haskell
λ> fmap (+1) $ Just 1
Just 2
λ> fmap (+1) Nothing
Nothing

λ> (+1) <$> Just 1
Just 2
λ> (+1) <$> Nothing
Nothing
```

```haskell
λ> fmap (++ " world!") (Just "Hello")
Just "Hello world!"
λ> fmap (++ " world!") (Nothing)
Nothing
```

```haskell
let mHello = Just "Hello"
case mHello of
  Nothing    -> print "Nothing"
  Just hello -> print (hello ++ " world!")
```

```
data Maybe a
data Either a b
data Tuple a b
data List a
data RoseTree a = RoseTree a [RoseTree a]
```

```
instance Functor RoseTree where
  fmap f (RoseTree a b) = RoseTree (f a) (fmap f <$> b)
```

## multiple fmapping

`fmap f <$>`

`(fmap . fmap) f`

`(fmap . fmap) f <$>`

`(fmap . fmap . fmap) f`

`[(1,2),(3,4)]`

`[Just 1, Just 2]`

`('a',[a])`


TripleTuple

Result a b = Error a | Success b


`fst`
`snd`
`maybe`
`either`

change an item in a container but leave it in the container


should follow these laws

```haskell
fmap id  ==  id
fmap (f . g)  ==  fmap f . fmap g
```

```haskell
class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const
```
