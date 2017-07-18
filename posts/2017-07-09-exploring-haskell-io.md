---
title: Exploring Haskell - ghc-prim and base IO
---
ghc-prim
ghc-base

tie to main object

https://hackage.haskell.org/package/base-4.9.1.0/docs/src/Unsafe.Coerce.html#unsafeCoerce

```haskell
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
type role IO representational
```


```haskell
data RealWorld
```

```haskell
data State# s
```

- `RealWorld` is a boxed type used as a type parameter by unboxed types ([GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:RealWorld)).
RealWorld is deeply magical. It is primitive, but it is not unlifted (hence ptrArg). We never manipulate values of type RealWorld; it's only used in the type system, to parameterise State#. 
- `State#` is unboxed type for concurrency ([GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:State-35-)).
State# is the primitive, unlifted type of states. It has one type parameter, thus State# RealWorld, or State# s, where s is a type variable. The only purpose of the type parameter is to keep different state threads separate. It is represented by nothing at all. 

## roles

`Data.Coerce`

```haskell
class Coercible a b

coerce :: Coercible * a b => a -> b
```

 Secondly, two instance exist for every newtype NT = MkNT T: 

```bash
λ> import Data.Coerce
λ> newtype IntWrapper = IntWrapper Int
λ> coerce (IntWrapper 1) :: Int
1
λ> :t coerce (IntWrapper 1) :: Int
coerce (IntWrapper 1) :: Int :: Int
```

check if two types have the same underlying representation

GND GeneralizedNewtypeDeriving

representational - allows coercion between the outer type when we the inner types are equal.
nominal - type parameter may no longer have equivalent run-time representation despite the arguments having equality
phantom -  Inferred when the type parameter doesn't affect the run-time representation of the outer type. 

role ordering `nominal > representational > phantom`

 The goal of the roles system is to track when two types have the same underlying representation. For example: 
 
 
## fixIO
 
```haskell
fixIO :: (a -> IO a) -> IO a
fixIO k = do
  m <- newEmptyMVar
  ans <- unsafeInterleaveIO (takeMVar m)
  result <- k ans
  putMVar m result
  return result
```
 
[](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/System.IO.html#fixIO)

unsafeInterleaveIO

`unsafeInterleaveIO` allows `IO` computation to be deferred lazily.
When passed a value of type `IO a`, the `IO` will only be performed
when the value of the `a` is demanded.  This is used to implement lazy
file reading, see 'System.IO.hGetContents'.

```haskell
{-# INLINE unsafeInterleaveIO #-}
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO m = unsafeDupableInterleaveIO (noDuplicate >> m)
```

https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.IO.Unsafe.html#unsafeInterleaveIO

-- We used to believe that INLINE on unsafeInterleaveIO was safe,
-- because the state from this IO thread is passed explicitly to the
-- interleaved IO, so it cannot be floated out and shared.
--
-- HOWEVER, if the compiler figures out that r is used strictly here,
-- then it will eliminate the thunk and the side effects in m will no
-- longer be shared in the way the programmer was probably expecting,
-- but can be performed many times.  In #5943, this broke our
-- definition of fixIO, which contains
--
--    ans <- unsafeInterleaveIO (takeMVar m)
--
-- after inlining, we lose the sharing of the takeMVar, so the second
-- time 'ans' was demanded we got a deadlock.  We could fix this with
-- a readMVar, but it seems wrong for unsafeInterleaveIO to sometimes
-- share and sometimes not (plus it probably breaks the noDuplicate).
-- So now, we do not inline unsafeDupableInterleaveIO.

unboxed tuple
```haskell
{-# NOINLINE unsafeDupableInterleaveIO #-}
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO (IO m)
  = IO ( \ s -> let
                   r = case m s of (# _, res #) -> res
                in
                (# s, r #))
```


https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.IO.Unsafe.html#unsafeDupableInterleaveIO


Ensures that the suspensions under evaluation by the current thread
are unique; that is, the current thread is not evaluating anything
that is also under evaluation by another thread that has also executed
'noDuplicate'.

This operation is used in the definition of 'unsafePerformIO' to
prevent the IO action from being executed multiple times, which is usually
undesirable.

```haskell
noDuplicate :: IO ()
noDuplicate = IO $ \s -> case noDuplicate# s of s' -> (# s', () #)
```

Unboxed tuples

`(# e_1, ..., e_n #)`

Unboxed tuples are used for functions that need to return multiple values, but they avoid the heap allocation normally associated with using fully-fledged tuples. When an unboxed tuple is returned, the components are put directly into registers or on the stack; the unboxed tuple itself does not have a composite representation. Many of the primitive operations listed in primops.txt.pp return unboxed tuples. In particular, the IO and ST monads use unboxed tuples to avoid unnecessary allocation during sequences of operations.

Values of unboxed tuple types are subject to the same restrictions as other unboxed types; i.e. they may not be stored in polymorphic data structures or passed to polymorphic functions.

The typical use of unboxed tuples is simply to return multiple values, binding those multiple results with a case expression, thus:


There is really only one way to "perform" an I/O action: bind it to Main.main in your program. When your program is run, the I/O will be performed. It isn't possible to perform I/O from an arbitrary function, unless that function is itself in the IO monad and called at some point, directly or indirectly, from Main.main.

https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#unboxed-tuples

Functor IO uses `(>>=)`
[](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Base.html)

Control.Monad.IO
Monads in which IO computations may be embedded. Any monad built by applying a sequence of monad transformers to the IO monad will be an instance of this class.

```haskell
returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO (\ s -> case m s of (# new_s, _ #) -> unIO k new_s)

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a

liftIO :: IO a -> State# RealWorld -> STret RealWorld a
liftIO (IO m) = \s -> case m s of (# s', r #) -> STret s' r

failIO :: String -> IO a
failIO s = IO (raiseIO# (toException (userError s)))
```



[](http://blog.ezyang.com/2011/05/unraveling-the-mystery-of-the-io-monad/)

[](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/src/GHC.Types.html)
[](https://ghc.haskell.org/trac/ghc/wiki/SafeRoles/RolesOverview)

[](https://wiki.haskell.org/IO_inside)

[](https://hackage.haskell.org/package/base-4.9.1.0/docs/System-IO.html)
[](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad-IO-Class.html)
