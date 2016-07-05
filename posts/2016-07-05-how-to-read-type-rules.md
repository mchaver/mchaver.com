How to Read Type Rules

a term denotes a mathematical object and a formula denotes a mathematical fact
terms appear as components of a formula
in Type Theory we see terms as the letters $M$ $N$

The following are terms:
$x$
$(x+1)$
$f(x)$
constant symbols, variables (identifiers) and function symbols

In this case $M$ is an arbitrary term that reduces to something of the type Int.
It could be a function, a constant or the value from an identifier


\\[ \\frac{\\Gamma_1 \\vdash e_1 : \\tau_1 ... \\Gamma_n \\vdash e_n : \\tau_n}{\\Gamma \\vdash e : \\tau} \\]

$\vdash M : int$ means "M is an integer expression", "M reduces to an int value",
  "M is an int term"

\\[ \\frac{}{\\vdash n : int} n \\in \\mathbb{Z} \\]
n is an integer expression for every $n \in \mathbb{Z}$
proof trees
\\[ \\frac{\\vdash M : int \\; \\vdash N : int}{\\vdash M + N : int} \\]
$M$ is an integer expression, $N$ is an integer expression, then $M + N$ is an integer expression.

\\[ \\frac{}{\\vdash true : bool}  \\frac{}{\\vdash false : bool} \\\\
    \\frac{\\vdash M : int \\vdash N : int}{ \\vdash M > N : bool }
    \\frac{\\vdash M : bool \\vdash N : B \\vdash N' : B}{\\vdash case M of \\{true. N, false. N'\\} : B}
\\]

$\vdash$ turnstile: yields, proves, satisfies, entails
separate typing assumptions from the typing judgement
$\vdash A$ I know A is true
$P \vdash Q$ From P, I know Q

\\[ x : int \\vdash x + 4 : int \\] once x has been defined by some integer, x + 4 is an integer expression

1. A, B, C range over types.
2. M and N range over terms (expressions).
3. x, y and z are identifiers.
4. a finite set of distinct identifiers with associated types are a typing context.
5. $\Gamma$ and $\Delta$ range over typing contexts.
6. $\Gamma,x : A$ to be read as $\Gamma$ extended with the identifier x of type A.
7. Any term that can be proved in the empty context ($\vdash M : A$) is said to be closed.

\\[ \\frac{\\Gamma \\vdash M : int \\qquad \\Gamma \\vdash N : int}{\\Gamma \\vdash M + N : int}\\]
typing context = typing environment
typing environment Gamma has M of int, typing environment Gamma has N of int, typing environment Gamma has
M + N of int

\\[ \\frac{\\Gamma \\vdash M : bool \\qquad \\Gamma \\vdash N : B \\qquad \\Gamma \\vdash N' : B}
          {\\Gamma \\vdash case \\; M \\; of \\{true. N, false. N'\\} : B}
\\]

\\[ \\frac
    {\\Gamma \\vdash : A \\Gamma,x : A \\vdash N : B }
    {\\Gamma \\vdash let \\; M \\; be \\; x. \\; N : B}
\\]
M is an expression of type A
there is a identifier x of type A
there is an expression N of type B
x is set to the expression of M (type A)
x can be used in type N, it will return type B

introduction rules of a type tell you how to form something of that type
elimination rules for a type tell us how to use something of that type
$\Gamma \vdash true : bool$ is an introduction rule
typing rule for conditionals is an elimination rule

Cartesian Product
\\[ \\frac
    {\\Gamma \\vdash M : A \\qquad \\Gamma \\vdash N : B}
    {\\Gamma \\vdash \\langle M,N \\rangle : A \\times B }
\\]
first projection
\\[ \\frac
    {\\Gamma \\vdash M : A \\times B}
    {\\Gamma \\vdash \\pi M : A}  
\\]

second projection
\\[ \\frac
    {\\Gamma \\vdash M : A \\times B}
    {\\Gamma \\vdash \\pi ' M : B}  
\\]

case pattern matching

\\[ \\frac
    {\\Gamma \\vdash M : A \\times B \\qquad \\Gamma, x : A, y : B \\vdash N : C}
    {\\Gamma \\vdash case \\; M \\; of \\; \\langle x,y \\rangle . N : C}
\\]

type 1: singleton set, nullary product (equivalent to multiplicate identity of 1)

\\[
\\frac{}{\\Gamma \\vdash \\langle \\rangle : 1}
\\]

\\[ \\frac
    {\\Gamma \\vdash M : 1 \\Gamma \\vdash N : C}
    {\\Gamma \\vdash case \\; M \\; of \\; \\langle \\rangle . N : C}
\\]

disjoint union

\\[
\\frac{\\Gamma \\vdash M : A}
      {\\Gamma \\vdash inl \\; M : A + B}
\\]

\\[
\\frac{\\Gamma \\vdash M : B}
      {\\Gamma \\vdash inr \\; M : A + B}
\\]


\\[
\\frac{\\Gamma \\vdash M : A + B \\qquad \\Gamma ,x : A \\vdash N : C \\qquad \\Gamma ,x : B \\vdash N' : C}
      {\\Gamma \\vdash case \\; M \\; of \\; \\{ inl \\; x. N, inr \\; x. N' \\} : C }
\\]

nullary disjoint union

\\[
\\frac{\\Gamma \\vdash M : 0}
      {\\Gamma \\vdash case \\; M \\; of \\; \\{\\} : A }
\\]

function introduction rule
\\[
\\frac{\\Gamma ,x : A \\vdash M : B}
      {\\Gamma \\vdash \\lambda x_A. M : A \\rightarrow B}
\\]

function elimination rule

\\[
\\frac{\\Gamma \\vdash M : A \\rightarrow B \\qquad \\Gamma \\vdash N : A}
      {\\Gamma \\vdash M \\; N : B}
\\]

[Typed Lambda Calculus](http://www.cs.bham.ac.uk/%7Epbl/mgs/lam/mgsbegin.pdf)
[Lecture Notes on the Lambda Calculus](http://www.mscs.dal.ca/~selinger/papers/lambdanotes.pdf)
[Turnstile](https://en.wikipedia.org/wiki/Turnstile_(symbol))
