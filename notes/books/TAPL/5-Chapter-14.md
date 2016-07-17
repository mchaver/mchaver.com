---
title: Chapter 14 - Exceptions
date:  2016-07-10
---

Raising and Handle Exceptions
Program needs to signal to its caller that it is unable to perform its task for
some reason (divide by zero, array index out of bounds, file not found, system
running out of memory). Some situations can be handled by returning a variant
object.

Exceptional condition causes a direct transfer of control to an exception handler
defined at some higher-level in the program or abort the program.

Error Evaluation Rules
E-AppErr1 $error \; t_2 \to error$
E-AppErr2 $v_1 \; error \to error$

Error Typing Rules
T-Error $\Gamma \vdash error : T$

If we encounter an error while reducing the left-hand side of an application to
a value, immediately yield error as the result of the application.

If we encounter an error while we are reducing the argument of an application of
a value, abandon work on the application on immediately yield an error.

Error is allowed to have any type in any context, but this creates some difficulties
when implementing the type checking algorithm. It breaks the property that every
typable term in the language has a unique type.

In a language with subtyping, assign error the minimal type Bot, and can be
promoted to any other type as necessary.

In a language with parametric polymorphism, give error the polymorphic type
$\forall X . X$, which can be instantiated to any other type.

Progress property must be altered to allow for error. Originally, a well-typed
program must evaluate to a value (or diverge). Now we have a non-value normal
form, error, which can be the result of a well-type program.

Theorem Progress: Suppose $t$ is a closed, well-typed normal form. Then either
$t$ is a value or $t = error$.

###Handling Exceptions
Evaluation rules for error can be thought of as "unwinding the call stack".
Discard pending function calls until the error has propagated all the way to the
top level. Call stack consists of a set of activation records, one for each
active function call, raising an exception cause activation records to be
popped off the call stack until it becomes empty.

Exception handlers in the call stack, when an exception is raised, activation
records are popped off the call stack until an exception handler is encountered
and evaluation then proceeds with the handler. Exception functions as a non-local
transfer of control, whose target is the most recently installed exception
handler.

###Exceptions Carrying Values
$T_{exn}$ exception type that carries a value.
