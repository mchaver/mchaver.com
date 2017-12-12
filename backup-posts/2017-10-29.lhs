---
title: Prelude Monads
tags: haskell, monad
---

\begin{code}
import Control.Applicative
import Control.Monad

data Option a = None | Some a

instance Functor Option where
  fmap _ None = None
  fmap f (Some a)  = Some . f $ a

instance Applicative Option where
  pure = Some

  Some f <*> m = fmap f m
  None <*> _m = None

--  liftA2 f (Some a) (Some b) = Some (f a b)
--  liftA2 _ _ _ = None

  Some _m1 *> m2 = m2
  None *> _m2 = None

instance Monad Option where
  (Some x) >>= k = k x
  None >>= _ = None

  (>>) = (*>)

  fail _ = None

father :: String -> Option String
father = undefined

mother :: String -> Option String
mother = undefined

maternalGrandmother p = mother p >>= father

bothGrandfathers p =
  father p >>=
    (\dad -> father dad >>=
      (\gf1 -> mother p >>=   -- gf1 is only used in the final return
        (\mom -> father mom >>=
          (\gf2 -> return (gf1,gf2) ))))


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f m = ReaderT $ \r -> fmap (\a -> f a) $ runReaderT m r

-- runReaderT m r
-- r is a value of type r
-- m is a ReaderT r m a, runReaderT returns a function (r -> m a) and applies it to r, returning a (m a)
-- then fmap (\...) (m a) returns $ ReaderT m (f a)


instance (Applicative m, Monad m) => Applicative (ReaderT r m) where
  pure a = ReaderT $ \_ -> return a

  f <*> v = ReaderT $ \r -> runReaderT f r <*> runReaderT v r

  u *> v = ReaderT $ \ r -> runReaderT u r *> runReaderT v r

  u <* v = ReaderT $ \ r -> runReaderT u r <* runReaderT v r

instance (Monad m) => Monad (ReaderT r m) where
    m >>= k  = ReaderT $ \ r -> do
      a <- runReaderT m r
      runReaderT (k a) r

--    (>>) = (*>)

    m >> k = ReaderT $ \ r -> runReaderT m r >> runReaderT k r

--    fail msg = lift (fail msg)



-- State Transformer environment

-- State has a function that goes from s to m (a, s)
-- s is that state that gets passed along, it can change
-- a, a is the new state
newtype State s m a = State { runState :: s -> m (a, s) }

-- runState takes a State function and a init state
-- m is the wrapper around the state, it must be a functor
-- apply f to a
instance (Functor m) => Functor (State s m) where
  fmap f m = State $ \s -> fmap (\(a, s') -> (f a, s')) $ runState m s


instance (Functor m, Monad m) => Applicative (State s m) where
  -- given a result state a, wrap it in a State
  pure a = State $ \ s -> return (a, s)

  -- given two states, apply the state from left to the state from the right

  State mf <*> State mx = State $ \ s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')

  m *> k = m >>= \_ -> k

instance (Monad m) => Monad (State s m) where
  m >>= k  = State $ \ s -> do
    (a, s') <- runState m s
    runState (k a) s'

  fail str = State $ \ _ -> fail str

main :: IO ()
main = pure ()

{-
(>=>) monadic composition operator
(f >=> g) >=> h = f >=> (g >=> h)
(>=>) :: Monad m -> (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = \x -> f x >>= g

(.)
(f <=< g)

apply a function to every element in a container
fmap :: (a -> b) -> f a -> f b
package element in container
return :: a -> m a
flatten container of conatiners
join :: m (m a) -> m a 
-}

\end{code}
