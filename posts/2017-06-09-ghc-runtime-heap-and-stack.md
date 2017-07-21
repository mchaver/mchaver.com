---
title: GHC Runtime - Stack and Heap
tags: haskell
---

Stack and heap are important data structures for program execution. First we 
will review the general design of stack and heap [[5]](https://stackoverflow.com/a/80113), 
[[6]](https://stackoverflow.com/a/79936).

## General Review of Heap and Stack 

Static object reside in the compiled code and are not moved. Dynamic objects 
reside in the heap and may be moved by a garbage collector (if that is part of 
the programming language).

### Stack
- stored in RAM
- static allocation
- last in first out (LIFO) structure
- faster to allocate in comparison to heap
- attached to a thread, each thread gets a stack, stack deallocates when thread is exited
- size is set when thread is allocated
- pointer adjustment to free a block
- stores local data, return addresses, and used for parameter passing
- very large allocations can cause stack overflow
- usually has a maximum size when the program starts
- each byte in the stack tends to be reused frequently, can be mapped to the processor's cache
- use stack if you exactly how much data there is before compiling and it is not too large

### Heap
- stored in RAM
- dynamic allocation
- in C++, variables on the heap must be destroyed manually before falling out of scope with `delete`, `delete[]` or `free`
- slower to allocate than stack
- size is set on application startup but can grow as needed, space reclaimed on application exit
- used on demand to allocate a block of data for use by the program
- may encounter fragmentation when there are lots of allocations and deallocations
- data created on the heap will be pointed to by pointers and allocated with `new` (C++) or `malloc` (C/C++)
- allocation failures if too big of a buffer is requested to be allocated
- can cause memory leaks
- generally needs be multi-threading safe (this slows down performance)
- use heap if you don't know how much memory you need at runtime or allocate a lot of data

## Features of the GHC Runtime system

- knows how to raise an exception when you call `error`
- code to allocate `Array#` 
- implements `takeMVar#`
- storage manager
- Multi-generational garbage collector with copying and compacting strategies
- user-space scheduler for Haskell threads
- Haskell threads across multiple CPUs
- allows Haskell threads to call foreign functions in separate OS threads 
- heap-profiling 
- time-profiling
- code coverage
- Software Transactional Memory (STM)

## GHC Runtime Heap and Stack

Objects on the stack and heap have almost the same structure. Each object starts 
with a header, which points to an info table with a closure, and then each
type of object has its own payload. 

Stack frame info tables have one extra field than heap info tables, SRT field, 
which points to the static reference that has information about what static 
objects are referenced by the closure.

There is one difference between heap object payloads and stack object payloads.
If a heap object payload has pointers and non-pointers, it will list all the 
pointers before all of the non-pointers. Stack frame objects may have mixed 
order pointers and non-pointers.

#### Objects

- header: has an info pointer that points to the info table for a closure. In profiling it also has who created the closure.
- payload: depends on object type, may include: arity, pointers, non-pointers, size, closure, etc.

#### Info Table

Contains all the information that runtime needs to know about closures.

- SRT (stack frame only): information about what static objects are referenced by the closure
- Layout Type: pointers-first layout (number of pointers and non-pointers) or bitmap layout (small or large)
- Closure Type: function, thunk, constructor, etc.
- SRT Bitmap: garbage collection of Constanct Applicative Forms (CAFs), static closure that is a thunk, e.g. `squares = map square [1..]`
- Entry Code: code that will evaluate the closure. However, in the case of functions, the entry code will apply the function to the arguments given in registers or on the stack.

#### Type of Objects 

List of object names and their payload structures. All objects start with a 
header and are then followed by a payload (structure differs between objects).

- Data Constructors: pointers-first layout (header, pointers, non-pointers)

- Function Closures: dynamic and static (header, pointers, non-pointers)

- Thunks: object not in normal form (header, empty, pointers, non-pointers)

- Selector Thunks (dynamically allocated): entry code performs selection operation from a data constructor drawn from a single-constructor type (header, selectee pointer)

- Partial Application: function with too few arguments (header, arity, number of words, function closure, payload)

- Generic Application: application of function to given arguments (header, arity, number of words, function closure, payload)

- Stack Application: computation of a thunk that was suspended mid way through (header, size, closure, payload)

- Indirections: closure that points to other closure (header, target closure)

- Byte-code Objects: bits of code that can be interpreted by GHC's byte-code interpreter

- Black Holes: thunk under evaluation by another thread (header, target closure)

- Arrays: non-pointer (header, bytes, array payload), pointer (header, pointers, size, array payload + card table)

- MVar: (header, header of queue, tail of queue, value)

- Weak pointer

- Stable name

- Thread State Objects: complete state of a thread, including its stack

- STM objects: used by STM library: `TVAR_WAIT_QUEUE`, `TVAR`, `TREC_CHUNK`, `TREC_HEADER`

- Forwarding Pointers: the new location for an object that has been moved by the garbage collector

#### Thread State Object

Every Thread State Object ([TSO](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects?redirectedfrom=Commentary/Rts/HeapObjects#ThreadStateObjects))
has a stack. TSOs are ordinary objects that live in the heap and can take 
advantage of existing allocation and garbage collection to manage them. Garbage 
collection can detect when a blocked thread is unreachable and make it never 
runnable again.

#### Boxed and Lifted Types

- Lifted type contains bottom `_|_`

- Unlifted type contains does not contain bottom

- Boxed type is represented by a pointer to an object on the heap

- Unboxed type represents a value (char, double, float, integer, etc.)

`Int` is boxed. `Int#` is unboxed. Unboxed types are suffixed with a `#`. The 
representation of bottom `_|_` is a pointer and when evaluated it will throw an 
exception or enter an infinite loop.


## References

- [[1] Edward Z. Yang :: The GHC Runtime System](http://ezyang.com/jfp-ghc-rts-draft.pdf)

- [[2] GHC Commentary :: The Layout of Heap Objects](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects)

- [[3] GHC Commentary :: The Layout of the Stack](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/Stack)

- [[4] Hackage :: stm](https://hackage.haskell.org/package/stm)

- [[5] Stack Overflow :: Brian R. Bondy :: What and where are the stack and heap?](https://stackoverflow.com/a/79936)

- [[6] Stack Overflow :: Jeff Hill :: What and where are the stack and heap?](https://stackoverflow.com/a/80113) 

- [[7] Takenobu T. :: GHC (STG, Cmm, asm) illustrated](https://takenobu-hs.github.io/downloads/haskell_ghc_illustrated.pdf)

- [[8] The Glasgow Haskell Compiler :: GHC Commentary: The Runtime System](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts)
