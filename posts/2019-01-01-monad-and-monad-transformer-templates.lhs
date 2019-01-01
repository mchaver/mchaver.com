---
title: Monad and Monad Transformer Templates
tags: haskell, monad, monad transformer
---

This is just a quick reference for implementing monad and monad transform type class instances. Monad transformers in particular require a lot of boilerplate code. Monad transformers allow us to combine multiple monads like `Maybe`, `Reader` and `IO` into a single monad stack and access the capabilities of each monad.
A common stack is `ReaderT Env IO`, where `Env` may contain mutable references.
This is generally recommended over using `WriterT` and `StateT` for performance and safety issues.
You can read more about best practices in this FPComplete article: [ReaderT Design Pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern).

== Monad Template

You should be generally familiar with how [Functor](https://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.Base.html#Functor), [Applicative](https://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.Base.html#Applicative) and
[Monad](https://hackage.haskell.org/package/base-4.10.1.0/docs/src/GHC.Base.html#Monad) are defined in GHC. Here is a slightly redacted version.

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  -- | Lift a value.
  pure :: a -> f a

  -- | Sequential application.
  (<*>) :: f (a -> b) -> f a -> f b
  (<*>) = liftA2 id

  -- | Lift a binary function to actions.
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f x = (<*>) (fmap f x)

  -- | Sequence actions, discarding the value of the first argument.
  (*>) :: f a -> f b -> f b
  a1 *> a2 = (id <$ a1) <*> a2
  
  -- | Sequence actions, discarding the value of the second argument.
  (<*) :: f a -> f b -> f a
  (<*) = liftA2 const

class Applicative m => Monad m where
  -- | Sequentially compose two actions, passing any value produced
  -- by the first as an argument to the second.
  (>>=)       :: forall a b. m a -> (a -> m b) -> m b

  -- | Sequentially compose two actions, discarding any value produced
  -- by the first, like sequencing operators (such as the semicolon)
  -- in imperative languages.
  (>>)        :: forall a b. m a -> m b -> m b
  m >> k = m >>= \_ -> k

  -- | Inject a value into the monadic type.
  return :: a -> m a
  return = pure

  -- | Fail with a message.
  fail :: String -> m a
  fail s = errorWithoutStackTrace s

join :: (Monad m) => m (m a) -> m a
join x =  x >>= id
```

These are my one sentence motivations (not definitions) for using these type classes in Haskell:

- Functor: apply a function to a value/values in a container without removing the container.

- Applicative: build values from independent computations.

- Monad: build values from interdependent computations.


Let's define our own type and instances for these three type classes. I use the naming scheme from OCaml `Option` so we can compile
this and avoid compile errors regarding `Maybe`.

\begin{code}
import Control.Applicative (liftA2, Alternative(..))
import Control.Monad (liftM, ap, MonadPlus(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..), lift)

data Option a = None | Some a deriving (Eq, Ord, Show)

instance Functor Option where
  fmap _ None     = None
  fmap f (Some a) = Some (f a)

instance Applicative Option where
  pure = Some

  Some f <*> m  = fmap f m
  None   <*> _m = None

  liftA2 f (Some x) (Some y) = Some (f x y)
  liftA2 _ _ _               = None

  Some _m1 *> m2  = m2
  None *> _m2     = None

instance Monad Option where
  (Some x) >>= k      = k x
  None     >>= _      = None
  (>>)                = (*>)
  fail _              = None
\end{code}

Here are some motivating functions for our Monad instance. We have three functions that we want to combine:

\begin{code}
residentToAddress :: String -> Option String
residentToAddress "Foo" = Some "10 Downing Street"
residentToAddress "Bar" = Some "1600 Pennsylvania Ave NW"
residentToAddress _     = None

addressToPhoneNumber :: String -> Option String
addressToPhoneNumber "10 Downing Street"        = Some "+44 20 7925 0918"
addressToPhoneNumber "1600 Pennsylvania Ave NW" = Some "+1 202-456-1111"
addressToPhoneNumber _                          = None

phoneNumberToAccountNumber :: String -> Option String
phoneNumberToAccountNumber "+44 20 7925 0918" = Some "1111-1111-1111-1111"
phoneNumberToAccountNumber "+1 202-456-1111"  = Some "2222-2222-2222-2222"
phoneNumberToAccountNumber _                  = None 
\end{code}

Without using monads it looks like this:

\begin{code}
residentToAccountNumber :: String -> Option String
residentToAccountNumber r =
  case residentToAddress r of
    Some a ->
      case addressToPhoneNumber a of
        Some p -> phoneNumberToAccountNumber p
        None -> None
    None   -> None
\end{code}

Using monads (particularly with do-syntax) we can simplify it:

\begin{code}
residentToAccountNumberDoSyntaxSugar :: String -> Option String
residentToAccountNumberDoSyntaxSugar r = do
  address <- residentToAddress r
  phoneNumber <- addressToPhoneNumber address
  residentToAccountNumber phoneNumber  

residentToAccountNumberNoSyntaxSugar :: String -> Option String
residentToAccountNumberNoSyntaxSugar r =
  residentToAddress r >>= \address -> addressToPhoneNumber address >>= \phoneNumber -> residentToAccountNumber phoneNumber
\end{code}

== Monad Transformer Template

\begin{code}
newtype OptionT m a = OptionT { runOptionT :: m (Option a) }

mapOptionT :: (m (Option a) -> n (Option b)) -> OptionT m a -> OptionT n b
mapOptionT f = OptionT . f . runOptionT

instance (Functor m) => Functor (OptionT m) where
  fmap f = mapOptionT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (OptionT m) where
  pure = OptionT . pure . Some

  mf <*> mx = OptionT $ do
      mb_f <- runOptionT mf
      case mb_f of
        None -> pure None
        Some f  -> do
          mb_x <- runOptionT mx
          case mb_x of
            None -> pure None
            Some x  -> pure (Some (f x))

  m *> k = m >>= \_ -> k

instance (Monad m) => Monad (OptionT m) where
  return = OptionT . pure . Some

  x >>= f = OptionT $ do
      v <- runOptionT x
      case v of
        None   -> pure None
        Some y -> runOptionT (f y)

  fail _ = OptionT (pure None)

instance MonadTrans OptionT where
  lift = OptionT . liftM Some

instance (MonadIO m) => MonadIO (OptionT m) where
  liftIO = lift . liftIO

instance Monad m => Alternative (OptionT m) where
  empty   = OptionT $ pure None
  x <|> y = OptionT $ do
              ov <- runOptionT x
              case ov of
                None   -> runOptionT y
                Some _ -> pure ov

instance Monad m => MonadPlus (OptionT m) where 
  mzero = empty
  mplus = (<|>)

residentToAddressMT :: IO () -> String -> OptionT IO String
residentToAddressMT action "Foo" = do liftIO action; OptionT . pure $ Some "10 Downing Street"
residentToAddressMT action "Bar" = do liftIO action; OptionT . pure $ Some "1600 Pennsylvania Ave NW"
residentToAddressMT action _     = do liftIO action; OptionT . pure $ None

residentToAccountNumberMonadTransformer :: (OptionT IO String)
residentToAccountNumberMonadTransformer = do
  r  <- liftIO getLine
  _a <- residentToAddressMT (print "printing from residentToAddressMT") r

  -- you can wrap pure Maybe Monad
  OptionT . pure $ do
    address     <- residentToAddress r
    phoneNumber <- addressToPhoneNumber address
    phoneNumberToAccountNumber phoneNumber

main :: IO ()
main = do
  oaccount <- runOptionT residentToAccountNumberMonadTransformer
  print oaccount
\end{code}
