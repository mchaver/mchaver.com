---
title: GHC Runtime Garbage Collection
---

Garbage collection is a form of memory management. It attempts to collect memory 
occupied by objects that are no longer used by the program. Many systems use a
combination of garbage collection and manual memory management. It may influence 
processing time.

stack allocation
region inference

Garbage collection may eliminate the following bugs:

- Dangling pointers: piece of memory is freed for which pointers still exist and then one of those pointers is dereference
- Double free: program tries to free a region of memory that has already been freed and perhaps reallocated
- Memory leak: program fails to free memory of unreachable objects, may lead to memory exhaustion.

consume additional resource, performance impact, stalls in program execution and 
incompatibility with memory management. 

## Strategies

### Tracing

Trace which objects are reachable by a chain of references from certain root 
objects.

### Reference Counting

Each object has a count of the number of references to it. If it has zero 
references then it is garbage. Guarantees that objects are destroyed as soon as
their last reference is destroyed.

### Escape Analysis

Convert heap allocations to stack allocations. Done via a compile-time Analysis
to determine whether an object allocated within a function is not accessible 
outside of it to other functions or threads. In this case it can be allocated 
directly on the thread stack.

[Wikipedia :: Garbage collection](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science))
[Wikipedia :: Generation garbage collection](https://en.wikipedia.org/wiki/Tracing_garbage_collection#Generational_GC_.28ephemeral_GC.29)

## Generational Garbage Collection 

A type of tracing garbage collection. 
Most recently created objects are also the most liekly to become unreachable 
quickly (empirical observation). Divide objects into generations and place only 
the objects of a subset of generation into the initial white (condemned) set. RS system 
has knowledge of when references cross generations by observing the creation and 
overwriting of references. Use this knowledge to prove some objects 
in the white set are unreachable without having to traverse the reference tree. 
separate memory regions for different ages of objects, when region is full, 
objects are traced using references from older gnereation as roots.
heuristic approach

## GHC Garbage Collection

Flexible with many ways to configure the behavior

## Aging

recently added objects have not had sufficient chance to die, promoting them to 
the next generation may lead to retention of unnecessary data. workse when objects 
are thunks that are subsequently updated.

stay in generation 0 for a while.

https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/GC

http://wiki.c2.com/?GenerationalGarbageCollection




























