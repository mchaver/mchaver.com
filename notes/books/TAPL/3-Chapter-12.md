---
title: Chapter 12 - Normalization
date:  2016-07-10
---

Fundamental property of simply typed lambda-calculus.
Normalization is a property of evaluation. The evaluation of a
well typed program is guaranteed to halt in a finite number of steps.
Every well-typed term is normalizable.

Does not extend to full blow programming languages. This property does
not hold with recursion and recursive types.

$R_T$: set of closed terms of type T.

 - $R_A(t)$ iff t halts.
 - $R_{T_1 \to T_2}(t)$ iff t halts and, whenever $R_{T_1}(s)$, thus $R_{T_2}(t \; s)$.

Lemma: If $R_T(t)$, then t halts.

Lemma: If $t : T$ and $t \to t'$, then $R_T(t)$ iff $R_T(t')$.

Lemma: if $x_1 : T_1, \ldots, x_n : T_n \; \vdash \; t : T$ and
$v_1 \ldots ,v_n$ are closed values of types $T_1 \ldots T_n$ with
$R_{T_i}(v_i)$ for each $i$, then $R_T([x_1 \mapsto v_1] \cdots [x_n \mapsto v_n]t)$

Normalization Theorem: if $\vdash t : T$, then $t$ is normalizable.
