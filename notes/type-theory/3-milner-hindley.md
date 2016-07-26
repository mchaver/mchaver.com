---
title: Milner-Hindley
date:  2016-07-20
---

$[Var]$
\\[
\\frac{x : \\sigma \\in \\Gamma}
      {\\Gamma \\vdash x : \\sigma}
\\]
$x$ has type $\sigma$ in context $\Gamma$, then we know that in $\Gamma$
$x$ has type $\sigma$.


$[App]$
\\[
\\frac{\\Gamma \\vdash e_0 : \\tau \\to \\tau' \\qquad \\Gamma \\vdash e_1 : \\tau}
      {\\Gamma \\vdash e_0 \\; e_1 : \\tau'}
\\]
There are two conditions: $e_0$ is a function of type $\tau \to \tau'$ and $e_1$
is a value of type $\tau$. When $e_0$ is applied to $e_1$, the resulting type
is $tau'$.

$[Abs]$
\\[
\\frac{\\Gamma,x : \\tau \\vdash e : \\tau'}
      {\\Gamma \\vdash \\lambda x . e : \\tau \\to \\tau'}
\\]

$\Gamma,x:\tau$ means the context made up of $\Gamma$ adn the judgement $x:\tau$.
If we know that the value $x$ has type $\tau$ and the expression $e$ has type
$\tau'$, we know the type of a function that takes $x$ and returns $e$.

$[Let]$
\\[
\\frac{\\Gamma \\vdash e_0 : \\sigma \\quad \\Gamma, x : \\sigma \\vdash e_1 : \\tau}
      {\\Gamma \\vdash let \\; x = e_0 \\; in \\; e_1 : \\tau}
\\]
Expression $e_1$ has type $\tau$ as long as $x$ has type $\sigma$, the a $let$
expression which locally binds $x$ to a value of type $\sigma$ will make $e_1$
have type $\tau$. A let statement lets you expand the context with a new
binding.

$[Inst]$
\\[
\\frac{\\Gamma \\vdash e : \\sigma' \\qquad \\sigma' \\sqsubseteq \\sigma}
      {\\Gamma \\vdash e : \\sigma}
\\]
Handles subtyping. If you have a value of type $\sigma'$ and it is a subtype of
$\sigma$ then that expression is also of type $\sigma$.

$[Gen]$

\\[
\\frac{\\Gamma \\vdash e : \\sigma \\qquad \\alpha \\notin free(\\Gamma)}
      {\\Gamma \\vdash e : \\forall \\alpha . \\sigma}
\\]

generalize types. If there is some variable $\alpha$ that is not free in
anything in your context, then it is safe to say that any expression whose
type you know $e : \sigma$ will have that type for any value of $\alpha$.

a free variable is a variable that is not introduced by a let-statement or a
lambda inside some expression, this expression now depends on the value of
the free variable from its context

The whole is an logical implication top half is the assumption and the bottom
half is the result.

$\Gamma \vdash \ldots$ means "the expression $\ldots$" when you know the types of
every expression in $\Gamma$."

$\vdash$ means that you can prove something. $\Gamma \vdash \ldots$ means "I can
prove $\ldots$ in a context $\Gamma$." Type judgements. proves, determines 

$x : \sigma$ $x$ has type $\sigma$.


$\sqsubseteq$ partial ordering relation











["What part of Milner-Hindley do you not understand?"](https://stackoverflow.com/questions/12532552/what-part-of-milner-hindley-do-you-not-understand)
