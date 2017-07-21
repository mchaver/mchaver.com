---
title: Exploring Haskell - ByteString (Strict)
tags: haskell
---

ByteString is one of the core GHC libraries (emphasis on GHC because it is not 
part of the [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)
and I am not sure if it is compatible with other compilers like Hugs).
The ByteString library is probably a dependency (directly or indirectly) for 
most Hackage libraries. `ByteString`s are highly efficient string types. A 
`ByteString` is a sequence of bytes (8-bit characters). There are strict (single 
large array) and lazy (call-by-need, good for streaming) varieties. For using the
`ByteString` library, the Haddock documentation is good to get started. We will 
briefly look at some of the source code for strict `ByteString`s to get a 
general understanding of the implementation. While
this level of understanding is not necessary for every day use of `ByteString`s, 
it should be useful for having a deeper understanding of the GHC compiler.

### Data.ByteString

Here is the definition of strict `ByteString` from [Data.ByteString](https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/src/Data-ByteString-Internal.html#ByteString) in version `0.10.8.1`.

```haskell
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length
```

### UNPACK Pragma

`ByteString` has a constructor `PS` that takes three values: a payload, an offset 
and a length. The first think I noticed were the [UNPACK pragmas](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#unpack-pragma).
This pragma unpacks the contents of a constructor field by removing one level of 
indirection, much like `newtype`. If passed to a non-strict function, 
the compiler will rebox these types. However, the `-O` compiler flag will try 
to eliminate as many reboxings as possible.

### Strict Bang

Next, each of these types are prefixed with a bang (exclamation point) `!`, 
making them all strict. Strict means that these values are all call-by-value 
as opposed to call-by-need (value). This helps eliminate thunks and can improve 
performance in some cases.

### Data.Int and Integer

The offset and length types are `Int`s ([Data.Int](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Int.html#t:Int)). 
`Int` is a fixed-precision integer type with guaranteed range of at least 
`-2^29` to `2^29 - 1`. This is different from `Integer` ([Prelude](http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html#t:Integer)), 
which has an arbitrary range, but probably has worse performance because it has 
no bounds.

### Word8

The last type is `ForeignPtr Word8`. The `ByteString` payload is a pointer to a 
single `Word8` in C. This is similar to an array in C. We need a pointer to a 
value of a certain type, current location in the array and the total length of 
an array. We will start with `Word8` because it is less complicated then 
`ForeignPtr`. Here is the definition from [GHC.Word](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Word.html#Word8).

```haskell
data {-# CTYPE "HsWord8" #-} Word8 = W8# Word#
```

### CTYPE Pragma

`Word8` is an 8-bit unsigned integer type. There are a few things here that may 
be confusing if you have never looked at the core Haskell or GHC libraries.
The `CTYPE` pragma associates `Word8` with a C type `HsWord8` (more details [here](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/ffi-chap.html?highlight=ctype#the-capi-calling-convention)).
We know somewhere in the GHC language code there is a C type name `HsWord8`.

### Boxed and Unboxed Types

Next, we see a hash sign prefixed to the end of the constructor `W8#` and the 
value `Word#`. In Haskell, the hash sign `#` as a suffix to a name is an [Unboxed type](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#unboxed-types-and-primitive-operations).
Unboxed means there is no pointer or heap allocation and the type is a raw 
machine or primitive type.  Most types in GHC are boxed, the values of that type 
are represented by a pointer to a heap object. Unboxed types cannot be defined 
in Haskell. They are built into the language and compiler. Unboxed types are 
also lifted, meaning they cannot have the value bottom `_`. `W8#` is a unboxed 
(primitive) constructor and `Word#` is an unboxed (primitive) type.
  
Many unboxed types in Haskell are simple bit-patterns such as `Int#`, `Float#`, 
and `Double#`, but there are also more complicated boxed types like `Array#`,
a pointer to a heap-allocated object because the value would be too large to 
store in a register.

### ForeignPtr

Finally, we will explore the definition of `ForeignPtr`, defined in
[GHC.ForeignPtr](https://hackage.haskell.org/package/base-4.9.1.0/docs/Foreign-ForeignPtr.html#t:ForeignPtr).

```haskell
data ForeignPtr a = ForeignPtr Addr# ForeignPtrContents

data ForeignPtrContents
  = PlainForeignPtr !(IORef Finalizers)
  | MallocPtr      (MutableByteArray# RealWorld) !(IORef Finalizers)
  | PlainPtr       (MutableByteArray# RealWorld)

data Finalizers
  = NoFinalizers
  | CFinalizers (Weak# ())
  | HaskellFinalizers [IO ()]
```

What is a finalizer?

> A finalizer is a routine that is invoked when the Haskell storage manager 
detects that - within the Haskell heap and stack - there are no more references 
left that are pointing to the ForeignPtr. Typically, the finalizer will, then, 
invoke routines in the foreign language that free the resources bound by the foreign object.

### GHC.Prim

There are a variety of unboxed and boxed types 

- `Addr#` is also an unboxed type that points to something outside the 
garbarge-collected heap 
([GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/src/GHC.Prim.html#Addr%23)). 

- `MutableByteArray#` is used for FFI data allocation
([GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:MutableArray-35-) and [memory allocation details](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/ffi-chap.html?highlight=mutablebytearray#memory-allocation)).

- `RealWorld` is a boxed type used as a type parameter by unboxed types ([GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:RealWorld)).

- `State#` is unboxed type for concurrency ([GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:State-35-)).

- `Weak#` is an unboxed weak pointer ([GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:Weak-35-)).

### IORef

`IORef` is a container for a mutable value ([GHC.IORef](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.IORef.html#IORef)).

```haskell
newtype IORef a = IORef (STRef RealWorld a)
```

### STRef

We have seem `RealWorld` before, which is a type used by a state thread. `STRef`
is mutable variable in a state thread `s` that contains a value of type `a` 
([definition](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.STRef.html#STRef)).

```haskell
data STRef s a = STRef (MutVar# s a)
```

### MultVar#

Finally, `MultVar#` is also defined in [GHC.Prim](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:MutVar-35-).
It is a single item mutable array.

### References

- [Glasgow Compiler Users Guide :: 10.2. Unboxed types and primitive operations](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#unboxed-types-and-primitive-operations)

- [Glasgow Compiler Users Guide :: 10.31.11. UNPACK pragma](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#unpack-pragma)

- [Glasgow Compiler Users Guide :: 11.1.5. The CAPI calling convention](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/ffi-chap.html?highlight=ctype#the-capi-calling-convention)

- [Glasgow Compiler Users Guide :: 11.2.3. Memory Allocation](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/ffi-chap.html?highlight=mutablebytearray#memory-allocation)

- [Hackage :: Data.Int.Int](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Int.html#t:Int)

- [Hackage :: GHC.ForeignPtr.ForeignPtr](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.ForeignPtr.html#ForeignPtr)

- [Hackage :: GHC.IORef.IORef](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.IORef.html#IORef)

- [Hackage :: GHC.Prim.Addr#](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/src/GHC.Prim.html#Addr%23)

- [Hackage :: GHC.Prim.MutableByteArray#](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:MutableArray-35-)

- [Hackage :: GHC.Prim.MutVar#](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:MutVar-35-)

- [Hackage :: GHC.Prim.RealWorld](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:RealWorld)

- [Hackage :: GHC.Prim.Weak#](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#t:Weak-35-)

- [Hackage :: GHC.Prim.Word#](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#g:4)

- [Hackage :: GHC.STRef.STRef](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.STRef.html#STRef)

- [Hackage :: GHC.Word.Word8](https://hackage.haskell.org/package/base-4.9.1.0/docs/src/GHC.Word.html#Word8)

- [Hackage :: Prelude.Integer](http://hackage.haskell.org/package/base-4.9.1.0/docs/Prelude.html#t:Integer)

- [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)

- [Wikipedia :: Weak reference](https://en.wikipedia.org/wiki/Weak_reference)
