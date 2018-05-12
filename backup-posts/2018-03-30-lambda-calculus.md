syntax | name | description
$x$ | variable | 

| syntax        | name           | description  |
| ------------- |-------------| -----|
| x | variable | parameter or mathematical value |
| (λx.M) | abstraction | function definition. M is a lambda term, x is bound in the expression|
| (M N) | spplication | Apply a function to an argument. M and N are lambda terms |

Reduction operations
| Operation | Name | Description |
| --- | --- | --- |
| (λx.M[x] -> (λy.M[y])) | alpha-conversion | Rename a bound variable in an expression. Avoid name collisions |
| ((λx.M) E) -> (M[x:=E]) | beta-reduction | substitute a bound variable by the argument expression in the body of the abstraction | 

α-conversion
β-reduction

De Bruijn indexing elminates the need for α-conversion.
Repeated application reduces it to beta normal form.

A valida lambda calculus is an expressions called a lambda term.
Inductive definition
- a variable x is a valid lambda term
- if t is a lambda term and x is a variable then (lx.t) ia lambda term called a lambda abstraction
- if t and s are lambda terms, then (ts) is a lambda term called an application.

lambda abstraction lx.t is a defintiion of an anonymous function that takes a single input x and substitutes it into the expression t. 

application ts represetns the application of a function an an input s.

alpha equivalence. lx.x and lyy are alpha equivalent lambda terms because they both represent the identity function. x and y are not alpha equivalent because they are not bound in a lambda abstraction.

free variables
variables not bound by a lambda abstraction

capture avoiding substitutions
t s and r are lambda terms
x and y are variable
t[x := r] substitute r for x in a capture avoiding manner
- x[x := r] = r
- y[x := r] = y if x neq y
- (ts)[x := r] = (t[x := r])(s[x := r])


https://en.wikipedia.org/wiki/Lambda_calculus