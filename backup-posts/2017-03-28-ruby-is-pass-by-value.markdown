---
title: Ruby is pass by value
---

Variables are always references to objects. If you need to avoid changing an
object you have to use the `dup` or `clone` method. However, the function only 
needs to pass the reference to the value, but what you operate on is the actual
object. If you mutate an object in a function, that object is mutated everywhere.

Ruby does not support passing by reference. 

[Is Ruby pass by reference or by value?](http://stackoverflow.com/questions/1872110/is-ruby-pass-by-reference-or-by-value)
