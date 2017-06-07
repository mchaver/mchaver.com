Lambda Calculus

Lambda calculus is a formal system to describe computations based on function
abstraction and application. It consists of three simple terms and all valid 
recursive combinations of those terms: variables, applications and lambda 
expressions. It can be used as a simple language for 
defining programming languages much like a Turing Machine. 

The lambda term $λx.x$ takes a variable $x$ and returns the variable $x$. This 
is equivalent to $id$ in Haskell. $λx.xa$ is the application of a function $λx.x$ to
a variable $a$ which gets reduced to $a$. In Haskell this is $(\x -> x) a$.

Lambda expressions are composed of:

    variables v1, v2, ..., vn, ...
    the abstraction symbols lambda 'λ' and dot '.'
    parentheses ( )

The set of lambda expressions, Λ, can be defined inductively:

    If x is a variable, then x ∈ Λ
    If x is a variable and M ∈ Λ, then (λx.M) ∈ Λ
    If M, N ∈ Λ, then (M N) ∈ Λ

Terms 

First we define the terms of lambda calculus

\begin{code}
module Main where
  
import Data.List (union,(\\))

type Sym = String

data Expr 
  = Var Sym
  | App Expr Expr
  | Lam Sym Expr
  deriving (Eq, Read, Show)  
\end{code}

In our $Expr$ data type, the $id$ function looks like this:

The lambda calculus $id$ function looksA simple example in traditional lambda calculus syntax . In our 
Haskell implementation it looks like:

\begin{code}
idExample :: Expr
idExample = App (Lam "x" $ Var "x")
\end{code}

A more complex reducible example: $(λx.λy.x) (λz.z)$ 
and in Haskell: $(\x -> \y -> x) (\z -> z)$

\begin{code}
reducibleExample :: Expr
reducibleExample = App (Lam "x" $ Lam "y" $ Var "x") (Lam "z" $ Var "z")
\end{code}

Substitution

Next we want to evaluate the expression. A β-reduction step can be performed 
anywhere a function meets an argument.

You may also see the term redex (reducible expression).

$(λx.e)a reduces to e[x:=a]$

$e[x:=a]$ means that all (free) occurrences of the variable x in the expression 
e are replaced by a. The other reduction that we can perform α-substitution, the 
renaming of a variable, λx.x can be changed to λy.y.

\begin{code}
subst :: Sym -> Expr -> Expr -> Expr
subst v x b = 
  -- replace all free occurrences of v by b inside x
  sub b 
  where
    -- b is variable, if i from (Var i) is the same symbol then sub with x
    -- otherwise no change
    sub e@(Var i) = if i == v then x else e
    
    -- b is an application of f on a, apply subst to both branches
    sub (App f a) = App (sub f) (sub a)
    
    -- b is a Lambda (symbol and expression)
    sub (Lam i e) = 
      if v == i
        -- the bound variable is the same as the one we are replacing,
        -- there can be no free occurrences inside it, return the lambda
        -- without any changes
        then Lam i e
        else
          if i `elem` fvx
            -- lambda bound variable is among the free variables in x 
            then
              -- keep adding "'" until the variable is unique and does
              -- not exist in freeVars of x
              let i' = cloneSym e i
                  e' = substVar i i' e
              in Lam i' (sub e')
            -- i is not in the list of already existin free variables
            -- recurse on the body
            else
              Lam i (sub e)
    fvx = freeVars x
    cloneSym e i = loop i
      where 
        loop i' = if i' `elem` vars then loop (i ++ "'") else i'
        vars    = fvx ++ freeVars e

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e
\end{code}


In WHNF we only need to make sure there the there's no redex along the "spine" 
of the expression, i.e., starting from the root and following the left branch 
in applications. Doing normal order reduction means that we do not evaluate 
anything inside the argument of a β redex before doing the reduction. It's 
sometimes called lazy evaluation, but I prefer to use that term for an 
implementation strategy for normal order reduction. We need to perform a 
β-reduction, i.e., if we have App (Lam v e) a we need to replace all (free) 
occurrences of the variable v by the argument a inside the lambda body e. 
That's what the subst function does.

\begin{code}
whnf :: Expr -> Expr
whnf ee = spine ee []
  where
    spine (App f a) as = spine f (a:as)
    spine (Lam s e) (a:as) = spine (subst s a e) as
    spine f as = foldl App f as
\end{code}

The free variables are those variables that occur in an expression, but are not 
bound in it. We simply collect the variables in a set (using a list as 
a set here), removing anything bound. 

\begin{code}
freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i e) = freeVars e \\ [i]
\end{code}

WHNF substitution



The subst function will replace all free occurrences of v by b inside x, i.e., b[v:=x].
The Var case is easy. If it's the variable we are replacing then replace else leave it alone.
The App case is also easy, just recurse in both branches. 
The Lam case has three alternative.
First, if the bound variable is the same as the one we are replacing then there 
can be no free occurrences inside it, so just return the lambda as is. Second, 
if the lambda bound variable is among the free variables in x we have a problem 
(see below). Third case, just recurse in the body. So, what about the case when 
the lambda bound variable occurs in x? Well, if we just blindly continue 
substitution the variable v inside x will no longer refer to the same thing; it 
will refer to the variable bound in the lambda. That's no good. 
For example, take the expression λx.((λy.λx.y)x), the β reduction gives 
λx.λx'.x (or similar), whereas doing the substitution blindly would give 
λx.λx.x. Which is wrong! But it's easy to fix, just conjure up a variable, 
i' that will not clash with anything (cloneSym does that). 
So we take the original identifier and tack on "'" until it fulfills our requirements.

The substVar function is a utility when we want to replace one variable with another.

Another useful thing to be able to do is to compare lambda expression for equality. 
We already have syntactic equality derived for Expr, but it is also very useful 
to be able to compare expressions modulo α-conversions. 
That is, we'd like λx.x to compare equal to λy.y. Let's call that function alphaEq. 

\begin{code}
alphaEq :: Expr -> Expr -> Bool
alphaEq (Var v) (Var v') = v == v'
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s e) (Lam s' e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False
\end{code}

Variables and applications just proceed along the structure of the expression. 
When we hit a lambda the variables might be different, so we do an 
α-conversion of the second expression to make them equal. As the final functions, 
we will do reduction to Normal Form (i.e., to a form where no redexes remain) 
and comparison of expressions via their normal forms. 

\begin{code}
nf :: Expr -> Expr
nf ee = spine ee []
  where 
    spine (App f a) as = spine f (a:as)
    spine (Lam s e) [] = Lam s (nf e)
    spine (Lam s e) (a:as) = spine (subst s a e) as
    spine f as = app f as
    app f as = foldl App f (map nf as)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)
\end{code}

both whnf and nf may fail to terminate because not all expressions have a normal form

The canonical non-terminating example is (λx.x x)(λx.x x) which has one redex, and doing the reduction produces the same term again. 

\begin{code}
[z,s,m,n] = map (Var . (:[])) "zsmn"
app2 f x y = App (App f x) y
zero  = Lam "s" $ Lam "z" z
one   = Lam "s" $ Lam "z" $ App s z
two   = Lam "s" $ Lam "z" $ App s $ App s z
three = Lam "s" $ Lam "z" $ App s $ App s $ App s z
plus  = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ app2 m s (app2 n s z)

-- betaEq (app2 plus one two) three 
-- will equal True
\end{code}
 
redex (reducible expression)

a  Variable A character or string representing a parameter or value.
(λx.M) Abstraction function definition. The variable x becomes bound in the expressio
(M N) Application apply a function to an argument. M ad N are lambda terms

(λx.λy.(λz.(λx.z x) (λy.z y)) (x y))

reduction operations

((λx.M) E) -> (M[x:=E]) β-reduction substitute the bound variable ty the argument expression in the body of the abstraction
(λx.M[x]) ->  (λy.M[y]) α-conversion renme the bound variables in the expression to avoid collisions

Repeated applications will produce beta normal form

A valid lambda calculus expression is called a lambda term.
a variable x is a valid lambda term
if t is a lambda term, and x is a variable then (λx.t) is a lambda term (lambda abstraction)
if t and s are lambda terms then (ts) is a lambda term (application).



Computing the Normal Form is similar to Weak Head Normal Form
but as we reconstruct expressions we make sure that all 
subexpressions have Normal Form as well.
Both Normal Form and Weak Head Normal Form may fail to 
terminate bceause not all expressions have a normal form.
Non-terminating example is (λx.x x)(λx.x x).

Church-Rosser theorem, if an expression has a normal form
then it is unique. 

\end{code}

http://augustss.blogspot.tw/2007/10/simpler-easier-in-recent-paper-simply.html
http://augustss.blogspot.tw/2007/11/some-lambda-calculus-examples-syntax-in.html
https://en.wikipedia.org/wiki/Lambda_calculus
https://en.wikipedia.org/wiki/Beta_normal_form
