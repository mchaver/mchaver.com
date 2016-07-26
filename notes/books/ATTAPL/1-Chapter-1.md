---
title: Chapter 1 - Substructural Type Systems
data: 2016-07-23
---

Substructural type systems augment type systems with the ability to control
the number and order of data or operations. Useful for controlling access to
resources such as files, locks and memory.

Most type systems allow unrestricted use of variables in a type checking context.

Exchange: the order in which variables are written down is irrelevant.

Weakening: adding extra, unneeded assumptions to a context, does not prevent
a term from type checking.

Contraction: if we can type check a term using two identical assumptions
($x_2 : T_1$ and $x_3:T_1$) then we can check the same term using a single
assumption.

Linear type systems: every variable is used exactly once. Allows
exchange and weakening, does not allow contraction.

Affine type systems: every variable is used at most once. Allows exchange and
weakening, does not allow contraction.

Relevant type systems: every variable is used at least once. Allows exchange and
contraction, does not allow weakening.

Ordered type systems: every variable is use exactly once and in the order they
are introduced. Does not allow exchange, contraction or weakening.

####Linear Type System
Deallocated data can never be used again. Ensure that objects are used once, and
after use the object can be safely deallocated.

#####Syntax
Type qualifier $q$ annotate introduction forms for all data structures.
The linear qualifier $lin$ indicates that the data structure will be used
exactly once. Linear values are deallocated right after use. Unrestricted
qualifier $un$ means the data structure has the normal behavior.
$split \; t_1 \; as \; x,y \; in \; t_2$ projects the first and second components
from the pair $t_1$ and calls them $x$ and $y$ in $t_2$. Extract two components
while only counting a single use of pair.

#####Typing
Unrestricted types may not contain linear data structures.
$free$ uses its argument and then deallocates it.

#####Algorithmic Checking
$\div$ context difference operator. Check that linear variables do not appear and
the removal of unrestricted variables.

#####Operational Semantics
A value is a pair of a qualifier together with some data (a prevalue). Context
defines the order of evaluation of terms, they specify places in a term where
a computation can occur.

####Sums and Recursive Types
Recursive types introduced with $roll_p t$ expression. P is the recursive pretype
the expression will assume. Not annotated with a qualifier. They take the qualifier
in the expression $t$. All recursive functions are unrestricted data structures.

####Arrays
Arrays are problematic. If access the $i^{th}$ element, then it must be removed.
Could also use an iterator that accesses each element once, but this is limiting.

####Ordered Type System
Foundation for managing memory allocated on the stack. Control the exchange
property, we are able to guarantee that certain values are used in a
first-in/last-out order.

Explicit sequence of operations $let \; x \; = \; t_1 \; in \; t_2$: evaluate
$t_1$, bind result to $x$, then evaluate $t_2$.

qualifier $ord$ marks data allocated on the stack. Only pairs and value with
base type to be stack-allocated
