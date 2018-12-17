---
title: Type-Level List Search
tags: haskell, type-level
---

We will take a quick look at how to handle element search in two kinds of type 
lists: cons style lists and native Haskell lists. We only need three language 
pragmas.

\begin{code}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
\end{code}

For the cons style list, we can actually build it without relying on 
`TypeOperators`. We will work with simple data types of kind `* -> *` so we 
can make a simple cons like list. They do not need constructors because we 
will not make any values.

\begin{code}
data A a
data B a
data C a

data TypeLevelTrue
data TypeLevelFalse
\end{code}

Next we define a type family to encapsulate the idea of equality between types.
We just pattern match on the types.

\begin{code}
type family ConsContains list (single :: *) where
\end{code}

The list is not empty and the `single` type is the same type as the item in 

\begin{code}
  ConsContains (x xs) (x ()) = TypeLevelTrue
\end{code}

The list is empty.

\begin{code}
  ConsContains ()     (x ()) = TypeLevelFalse
\end{code}

The list is not empty, but it does not match the type on the right-hand side. 
Run `ConsContains` again but without the head of the list.

\begin{code}
  ConsContains (x xs) (y ()) = ConsContains xs (y ())
\end{code}

This function will only compile if you pass a type that is in the cons list. It 
uses `~` to try to match `TypeLevelTrue` with the result of `ConsContains`.

\begin{code}
consContains :: (TypeLevelTrue ~ ConsContains (A (B ())) a) => a -> IO ()
consContains _ = print "Type found in cons list."
\end{code}

This function will only compile if you pass a type that is not in the cons list.
The same idea as above but it tries to match `TypeLevelFalse`.

\begin{code}
consDoesNotContain :: (TypeLevelFalse ~ ConsContains (A (B ())) a) => a -> IO ()
consDoesNotContain _ = print "Type not in the cons list."
\end{code}

Now we will do the same thing but use native Haskell type level lists.

\begin{code}
data X
data Y
data Z
\end{code}

This is quite similar to the code above with a few small changes. The list type 
is now a list of kind`*`, written as `[*]`. We need to use `'` to promote the 
list splitter `:` to a type level list splitter `':`. Then we return type level 
boolean values `'True` and `'False`.

\begin{code}
type family Contains (list :: [*]) (single :: *) where
  Contains (x ': xs) (x) = 'True
  Contains '[]       (x) = 'False
  Contains (x ': xs) (y) = Contains xs (y)
\end{code}

Now we can use lists instead of types that are embedded in each other. A type 
level list needs to be declared with `'[]` and the result of `Contains` should 
match `'True` or `'False`.

\begin{code}
contains :: ('True ~ Contains '[X,Y] a) => a -> IO ()
contains _ = print "Type found in list."

doesNotContain :: ('False ~ Contains '[X,Y] a) => a -> IO ()
doesNotContain _ = print "Type not in list."
\end{code}

All of the uncommented code will compile.

\begin{code}
main = do
  consContains   (undefined :: A ())
  -- consContains   (undefined :: C ()) -- the compiler will not compile this
  consDoesNotContain (undefined :: C ())
  --  conDoesNotContain   (undefined :: A ()) -- the compiler will not compile this
  contains       (undefined :: Y)
  -- contains       (undefined :: Z) -- the compiler will not compile this
  doesNotContain (undefined :: Z)
  -- doesNotContain (undefined :: Y) -- the compiler will not compile this
\end{code}

If you uncomment any of the other lines you will see an error like this.

```bash
• Couldn't match type ‘TypeLevelTrue’ with ‘TypeLevelFalse’
    arising from a use of ‘consContains’
• In a stmt of a 'do' block: consContains (undefined :: C ())
  In the expression:
    do { consContains (undefined :: A ());
         consContains (undefined :: C ());
         consDoesNotContain (undefined :: C ());
         contains (undefined :: Y);
         .... }
  In an equation for ‘main’:
      main
        = do { consContains (undefined :: A ());
               consContains (undefined :: C ());
               consDoesNotContain (undefined :: C ());
               .... }
```
