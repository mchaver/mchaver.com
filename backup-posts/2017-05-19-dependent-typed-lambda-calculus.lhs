\begin{code}
module Main where
import Data.List (union, (\\))  

type Sym = String

type ErrorMsg = String
type TC a = Either ErrorMsg a

newtype Env = Env [(Sym, Type)] deriving (Show)

data Expr
    = Var Sym
    | App Expr Expr
    | Lam Sym Type Expr
    | Pi  Sym Type Type
    | Kind Kinds
    deriving (Eq, Read, Show)
type Type = Expr

data Kinds = Star | Box deriving (Eq, Read, Show)
\end{code}

\begin{code}
freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i t e) = freeVars t `union` (freeVars e \\ [i])
freeVars (Pi i k t) = freeVars k `union` (freeVars t \\ [i])
freeVars (Kind _) = []

subst :: Sym -> Expr -> Expr -> Expr
subst v x = sub
  where
    sub e@(Var i) = if i == v then x else e
    sub (App f a) = App (sub f) (sub a)
    sub (Lam i t e) = abstr Lam i t e
    sub (Pi i t e) = abstr Pi i t e
    sub (Kind k) = Kind k
    fvx = freeVars x
    cloneSym e i = loop i
       where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
             vars = fvx ++ freeVars e
    abstr con i t e =
        if v == i then
            con i (sub t) e
        else if i `elem` fvx then
            let i' = cloneSym e i
                e' = substVar i i' e
            in  con i' (sub t) (sub e')
        else
            con i (sub t) (sub e)
\end{code}

\begin{code}
nf :: Expr -> Expr
nf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s t e) [] = Lam s (nf t) (nf e)
        spine (Lam s _ e) (a:as) = spine (subst s a e) as
        spine (Pi s k t) as = app (Pi s (nf k) (nf t)) as
        spine f as = app f as
        app f as = foldl App f (map nf as)
\end{code}

\begin{code}
tCheck :: Env -> Expr -> TC Type
tCheck r (Var s) =
    findVar r s
tCheck r (App f a) = do
    tf <- tCheckRed r f
    case tf of
     Pi x at rt -> do
        ta <- tCheck r a
        when (not (betaEq ta at)) $ throwError $ "Bad function argument type"
        return $ subst x a rt
     _ -> throwError $ "Non-function in application"
tCheck r (Lam s t e) = do
    tCheck r t
    let r' = extend s t r
    te <- tCheck r' e
    let lt = Pi s t te
    tCheck r lt
    return lt
tCheck _ (Kind Star) = return $ Kind Box
tCheck _ (Kind Box) = throwError "Found a Box"
tCheck r (Pi x a b) = do
    s <- tCheckRed r a
    let r' = extend x a r
    t <- tCheckRed r' b
    when ((s, t) `notElem` allowedKinds) $ throwError "Bad abstraction"
    return t
\end{code}

\begin{code}
allowedKinds :: [(Type, Type)]
allowedKinds = [(Kind Star, Kind Star), (Kind Star, Kind Box), (Kind Box, Kind Star), (Kind Box, Kind Box)]
\end{code}
