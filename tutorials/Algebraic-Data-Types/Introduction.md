Algebraic Data Types in Haskell support full parametric polymorphism

algebra is a set of elements and some operations which map elements to elemnts.
disjoint union of simple types
vertical bar in haskell denotes union, often used for a union of similar types.
important properties: pattern matching, tags ofn the left to match and perform certain behvaior.
parametric, depend on other types. Allows us to create generic structures for modularity and 

`*` as tupling function
`+` as union function

`data Maybe a = Just a | Nothing`

product of types is a compouneded type in a structure, the operands of the product are types and the structure of a product type is determined by the fixed order of the operands in the product. 

Parametric Data Types, a data type that has a parameter that can be any other type

ADT are sum types
Constructors are product (tuple fields)

correspond to an initial algebra in cateogry theory, provide some laws and operations and symobls to maniplate

use algebraic notation to describe regular data structures

`+` sum types (Either, Maybe, List)
`•` product types (tuples, structs)
`X` singled type (`data X a = X a`)
`1` for the unit type
`μ` lease fixed point (recursive types), usually implicit
`X²` for `X•X`

Units:
data () = ()
1
Options: data Maybe a = Nothing | Just a
1 + X
Lists data [a] = [] | a : [a]
L = 1+X•L
Binary Trees: data BTree a = Empty ( Node a (BTree a) (BTree a)
B = 1 + X•B²

Expansion: unfolding the fix point can be helpful for thinking about lists. L = 1 + X + X² + X³ + ... (that is, lists are either empty, or they have one element, or two elements, or three, or ...)

Composition, ◦, given types F and G, the composition F ◦ G is a type which builds “F-structures made out of G-structures” (e.g. R = X • (L ◦ R) ,where L is lists, is a rose tree.

Differentiation, deriviated of a data type D is the type of D structures with a single hole
1' = 0
X' = 1
(F + G)′ = F' + G′

(F • G)′ = F • G′ + F′ • G

(F ◦ G)′ = (F′ ◦ G) • G′



relational algebra

    projection (π) create a new relation from the existing one by removing columns, 
    selection(σ) create a new relation from an existing one by removing rows that satisfy a predicate function,  
    joining (⋈) create a new relation from two existing ones by matching up the domains and values of a chosen column,
    renaming (ρ) create a new relation from an existing one by renaming a column


Quotient Type: equality relation has been redefined sucht that the elemnts of the type are partitioned into a set of equivalence class whose cardinality is less than or equal to the base type. set-theoretic quotients, sets whose element are surjectively partitioned into equivalence classes by a given equivalence relation on the set.  rational numbers

Haskell Math Notes
`data Void` `0` It is impossible to construct a term of this type
`data Unit = Unit` `1` Type with only one term
`data Bool = True | False` `1 + 1` Type with two terms
`data Maybe a = Just a | Nothing` `a + 1` Read + as Either, Type with one term and one parametric term
`data Either a b = Left a | Right b` `a + b` both terms are parametric
`data (a, b) = (a, b)` `a × b` read as and
`a -> b` `b​^a` function

Void Zero constantly void type constrcutor, no structure no matter what label it is given
One yields a single unit structure, no parameters
singleton X sturcutre with a single location, paraemter
sum (disjoint (tagged) union)
least fixed points m, [] in list
implicit function theorem

hole - location not containing any data. derivative of D is type D with a single hole. Zipper can track a movable foxus, dummy label correspond to the hole

0 ways to construc Void
Unit is 1
Bool is 2
a + b inhabitants
(a, b) has an inhabitatin for each combination of a b
b ^ a inhabitatns data Tri = One | Two | Three  Tri -> Bool is eight, Bool -> Tri is nine


bottom, non termination ⊥

[StackOverflow - Haskell's Algebraic Data Types, Don Stewart May 6 2011 ](http://stackoverflow.com/a/5917133)

[Introduction to Programming Languages/Algebraic Data Types](https://en.wikibooks.org/wiki/Introduction_to_Programming_Languages/Algebraic_Data_Types)

[Species and Functors and Types, Oh My!, Brent A. Yorgey, Haskell’10, September 30, 2010, Baltimore, Maryland, USA](http://repository.upenn.edu/cgi/viewcontent.cgi?article=1774&context=cis_papers)


Clowns to the left of me, jokers to the right (Dissecting Data Structures), Conor McBride POPL 2008


[What the Heck are Algebraic Data Types? (for Programmers) David Eklun December 28 2011](http://merrigrove.blogspot.tw/2011/12/another-introduction-to-algebraic-data.html)

[The algebra (and calculus!) of algebraic data types Joel Burget](https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types)

[Simple Algebraic Data Types Bartosz Milewski January 13, 2015](https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/)

[Hussling Haskell types into Hasse diagrams December 6 2010 Edward Z. Yang](http://blog.ezyang.com/2010/12/hussling-haskell-types-into-hasse-diagrams/)