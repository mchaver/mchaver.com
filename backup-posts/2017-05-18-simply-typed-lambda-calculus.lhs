\begin{code}
module Main where
  
data Type = Base | Arrow Type Type
  deriving (Eq, Read, Show)
\end{code}

\begin{code}
type Sym = String

data Expr
  = Var Sym
  | App Expr Expr
  | Lam Sym Type Expr
  deriving (Eq, Read, Show)
\end{code}

\begin{code}
newtype Env = Env [(Sym,Type)] deriving (Eq, Read, Show)

initialEnv :: Env
initialEnv = Env []

extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env ((s, t) : r)
\end{code}

\begin{code}
type ErrorMsg = String

type TypeCheck a = Either ErrorMsg a
\end{code}

\begin{code}
findVar :: Env -> Sym -> TypeCheck Type
findVar (Env r) s =
  case lookup s r of
    Just t  -> return t
    Nothing -> throwError $ "Cannot find variable " ++ s
\end{code}

\begin{code}
typeCheck' :: Env -> Expr -> TypeCheck Type
typeCheck' r (Var s) =
  findVar r s
typeCheck' r (App f a) = do
  tf <- typeCheck r f
  case tf of
    Arrow at rt -> do 
      ta <- typeCheck r a
      when (ta /= at) $ throwError "Bad function argument type"
      return rt
    _ -> throwError "Non-function in application"
typeCheck' r (Lam s t e) = do
  let r' = extend s t r
  te <- typeCheck r' e
  return $ Arrow t te

typeCheck :: Expr -> Type
typeCheck e =
  case typeCheck' initialEnv e of
    Left msg -> error ("Type error:\n" ++ msg)
    Right t  -> t
\end{code}
