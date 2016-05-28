GADT Generalized Algebraic Datatypes

Algebraic Datatypes (ADT)
Parametric Algebraic Data Types

sum is alteration between A | B, A or B but not buth
prdoucti is a combintation A B means both A and B

Explicitly write down the types of the ocnstructors. Good for domain specific languages.

Phantom Types

```haskell
data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
```

GADTs help restrict the type of a constructor and the functions can take advantage of the type marker

all constructors of a data type must return the same type on the left hand side.

```
data Foo a = Boo a
 
```

have the terms carry around more type information

Phantom Type a type for which no data member is ever created.

Existential type a type whose identity cannot be determined, know to exist and may have certain properties (be in a typeclass)

Witness type a type whose existence is used to prove (stand as a witness) that something is true. Must be phantom

Higher-order abstract syntax in a type safe fashion
HOAS abstract syntax tress for languages with variable binders

Weak approximation of inductive families from dependently typed languages (Agda)
modify the return type based on the particular value constructor used

stronger type enforced guarantees than regular ADTs. type indexing

scrutinee type

Darcs 
HaskellDB



Haskell/GADT wikibooks https://en.wikibooks.org/wiki/Haskell/GADT

Fun with Phantom Types R. Hinze http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf

https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#generalised-algebraic-data-types-gadts

https://wiki.haskell.org/Algebraic_data_type
https://en.wikipedia.org/wiki/Algebraic_data_type

Implementing the darcs patch formalism ...and verifying it David Roundy February 2006 Cornell University
http://physics.oregonstate.edu/~roundyd/talks/fosdem-2006.pdf

https://en.wikipedia.org/wiki/Generalized_algebraic_data_type

https://en.wikipedia.org/wiki/Higher-order_abstract_syntax


Real World use of GADT http://stackoverflow.com/a/21189680

Pointwise Generalized Algebraic Data Types https://5900f98f-a-62cb3a1a-s-sites.googlegroups.com/site/chklin/research/pointwise-tldi10.pdf?attachauth=ANoY7crIf9FQLoF5m6p1eazz56Z9Kopg8eiYj18x_nYTY7FjwMtP9aLaakHoiJ6rdkUBerJtx4jj27duftBjI6iPMamrNfXelfd3BpVAIhkfSLGAGzlFDLkAat6WViDeoIzDF12ROAKcY9TP_2olT_rI-pn2fmwhiAhoAI5GAMnYQvcZwHwzk49fMqYs63L3wrz81Cp2VHE1xg-O5h0wpAu8HyhEXcCVw_oCzGgpyl3L_spUToQe4Oo%3D&attredirects=0