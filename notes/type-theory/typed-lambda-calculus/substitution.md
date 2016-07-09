Typed Lambda-calculus: Substitution and Equations

1 Substitution again
1.1 Substitutions and Renamings
Turn $\Gamma \vdash M : B$ into a context $\Delta$ by replacing identifiers.
Substitution from $\Gamma$ to $\Delta$ is a function $k$ taking each identifier
$x : A$ in $\Gamma$ to a term $\Delta \vdash k(x) : A$
$k*M$ the result of replacing all the free identifiers in $M$ according to $k$

renaming maps each identifier to an identifier

If $\Gamma \in \Gamma '$ and $\Gamma \vdash M : A$ then $\Gamma ' \vdash M : A$
