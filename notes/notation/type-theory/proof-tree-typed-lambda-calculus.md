$A$,$B$,$C$: unknown types or symbols that range over types.

$M$,$N$: unknown terms/expressions or symbols that range over types/expressions.

$x$,$y$,$z$: identifiers or variables (this is a bit of a misnomer because they do not change value).

$\Gamma$,$\Delta$: typing contexts. Finite sets of distinct identifiers and their associated types.

$Terms$,$Expressions$: constants, identifiers and functions such as $1$, $x$, $(x+1)$, $f(x)$.

$\vdash$,$Turnstile$: separate the typing assumption (left-hand side) from the typing judgement (right-hand side).
The left-hand side can be read as "from..", "assumming..". The right-hand side can be read as "It is known", "I know", "Then".

$\vdash M : int$ : $M$ is an integer expression or "I know M is an integer expression"

$M[N/x]$: substitute $N$ for $x$ in $M$ or any $x$ identifier in $M$ will have the value of $N$.

$\Gamma,x : A$: extend the typing context $\Gamma$ with an identifier $x$ of type $A$.

$Closed \; Term$: any term that can be proven in the empty context $\vdash M : A$.

$\alpha -equivalent$: two terms which share the same binding diagram.
$x : int, y : int \vdash (x+y) + let \; 3 \; be \; y. (x+y) : int$ and
$x : int, y : int \vdash (x+y) + let \; 3 \; be \; z. (x+z) : int$ are $\alpha -equivalent$

$\beta -reduction$

$\mapsto$

$M \rightsquigarrow N$: $M$ can be simplified to $N$. Introduction then elimination.
  $\pi \langle M,M' \range \rightsquigarrow M$
$\eta -expansion$: apply elimination then an introduction.
  $N[M/z]$ to $case \; M \; of \; {true. N[true/z],false. N[false/z]}$

A substitution from $\Gamma$ to $\Delta$ is a function $k$
taking each identifier $\Delta \vdash k(x) : A$ to a term
$\Delta \vdash k(x): A$.

$k*M$: result of replacing all the free identifiers in $M$ according to $k$.

$Syntactic \; Environment$: for $\Gamma$ provides,
for each identifier $x : A$ in $\Gamma$,
a closed term of type $A$.
(substitution from $\Gamma$ to the empty context).

$[A]$ : set
$Semantic \; Environment$: for $\Gamma$ provides, for each identifier
$x : A$ in $\Gamma$, an element of [A]

$[ \Gamma ]$: set of semantic environments for $\Gamma$.

$\Gamma \vdash M : B$
denotation of $M$ provides an element $[M]\rho \in [B]$,
for each semantic environment $\rho \in [ \Gamma ]$

$\llbracket a+b\rrbracket$

denotation = mathematical object (the meaning of the program)
mathematical object for all of these programs $10$ $9+1$ $2*5$ $sum [1..4]$
is $10$
collection of denotations (mathematical objects) is the semantic domain

$\llbracket M \rrbracket$: denotation (mathematical object) of expression $M$
