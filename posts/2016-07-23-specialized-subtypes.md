Full subtyping can be problematic for languages that are concerned about type
safety. However, reduced versions of subtyping can be useful without removing
the benefits of avoiding subtyping altogether.

Type Inference
Polymorphic Parameters
subtype polymorphism
width subtyping on type varaible constraint sets

subsumption (System F)
forall a. a -> a is a generic instance of Int -> Int because you can instantiate
a to Int

Type constructor builds new types from old ones
basic types built using nullary type constructors

```haskell
data Message =
  Error    String Int |
  Message  String String
```
ErrorSubMessage and MessageSubMessage types that are both their own types and
equivalent to the Message type. The poses problems

arbitrary type that returns uses only one constructor

constructor as function 


[](https://www.reddit.com/r/haskell/comments/423o0c/why_no_subtypingsubtype_polymorphism/)
[Making Sense of Subtypes](http://languagengine.co/blog/making-sense-of-subtypes/)
[Integrating Nominal and Substructural Subtyping](https://www.cs.cmu.edu/%7Edonna/public/ecoop08.pdf)
