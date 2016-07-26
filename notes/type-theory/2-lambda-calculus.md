variables: $v_1,v_2,...,v_n$
abstraction symbols: $\lambda$ and $.$
parentheses: $()$

$\Lambda$: set of lambda expressions
defined inductively
1. If $x$ is a variable, then $x \in \Lambda$
2. If $x$ is a variable and $M \in \Lambda$, then $(\lambda x . M) \in \Lambda$
3. If $M,N \in \Lambda$, then $(M \; N) \in \Lambda$

Free and bound variables
$\lambda$ binds its variables wherever tit occurs in the body of the abstraction.
Variables in the scope of an abstraction are said to be bound. All other variables
are free. In $\lambda y. x x y.$ the variable $x$ is free and $y$ is bound.

Reduction
α-conversion: changing bound variables (alpha);
β-reduction: applying functions to their arguments (beta);
η-conversion: which captures a notion of extensionality (eta).

Normalization

Semantics

[Lambda Calculus Definition](https://en.wikipedia.org/wiki/Lambda_calculus_definition)
