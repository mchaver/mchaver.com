

\begin{code}
{-# LANGUAGE DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module TinyServant where

import Control.Applicative
import GHC.TypeLits
import Text.Read
import Data.Time
import Data.Proxy
\end{code}


\begin{code}
data Get (a :: *)
\end{code}

Get can take any single kinded type. The type cannot have any polymorphic variables like `Maybe`, but
it can be `Maybe Text`. It computes a new type at compile time.

The sequential operator, called sub, aka bird-face: :>
The alternative operator: open mouth bird face. :<|>

\begin{code}
data a :<|> b = a :<|> b
infixr 8 :<|>
\end{code}

`:>`

\begin{code}
data (a :: k) :> (b :: *)
infixr 9 :>
\end{code}

Same as `Get`.

\begin{code}
data Capture (a :: *)
\end{code}



\begin{code}
--p = "person" :> Get Person

--c = Capture Currency :> Get Amount

type MyAPI = "date" :> Get String
        :<|> "time" :> Capture String :> Get String

serve :: HasServer layout
      => Proxy layout -> Server layout -> [String] -> IO String
serve p h xs = case route p h xs of
  Nothing -> ioError (userError "404")
  Just m  -> m

type family Server layout :: *

type instance Server (Get a) = IO a

--type instance Server (a :<|> b) = (Server a, Server b) -- preliminary

type instance Server (a :<|> b) = Server a :<|> Server b
type instance Server ((s :: Symbol) :> r) = Server r
type instance Server (Capture a :> r) = a -> Server r

class HasServer layout where
  route :: Proxy layout -> Server layout -> [String] -> Maybe (IO String)

type instance Server (Get a) = IO a

instance Show a => HasServer (Get a) where
  route :: Proxy (Get a)
        -> IO a -> [String] -> Maybe (IO String)
  route _ handler [] = Just (show <$> handler)
  route _ _       _  = Nothing
  
  
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  route :: Proxy (a :<|> b)
        -> (Server a :<|> Server b) -> [String] -> Maybe (IO String)
  route _ (handlera :<|> handlerb) xs =
        route (Proxy :: Proxy a) handlera xs
    <|> route (Proxy :: Proxy b) handlerb xs


instance (KnownSymbol s, HasServer r) => HasServer ((s :: Symbol) :> r) where
  route :: Proxy (s :> r)
        -> Server r -> [String] -> Maybe (IO String)
  route _ handler (x : xs)
    | symbolVal (Proxy :: Proxy s) == x = route (Proxy :: Proxy r) handler xs
  route _ _       _                     = Nothing


instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  route :: Proxy (Capture a :> r)
        -> (a -> Server r) -> [String] -> Maybe (IO String)
  route _ handler (x : xs) = do
    a <- readMaybe x
    route (Proxy :: Proxy r) (handler a) xs
  route _ _       _        = Nothing

main :: IO ()
main = return ()
\end{code}

[](https://www.well-typed.com/blog/2015/11/implementing-a-minimal-version-of-haskell-servant/)
[](http://languagengine.co/blog/making-sense-of-subtypes/)
[](https://wiki.haskell.org/Keywords#-.3E) - functional dependencies, view patterns
[](https://wiki.haskell.org/Keywords)
[](https://www.haskell.org/onlinereport/haskell2010/)


Type family declarations have no right-hand side, but GHC must still infer a kind for F. Since there are no constraints, it could infer F :: forall k1 k2. k1 -> k2, but that seems too polymorphic. So GHC defaults those entirely-unconstrained kind variables to * and we get F :: * -> *. You can still declare F to be kind-polymorphic using kind signatures:

```
type family F1 a                -- F1 :: * -> *
type family F2 (a :: k)         -- F2 :: forall k. k -> *
type family F3 a :: k           -- F3 :: forall k. * -> k
type family F4 (a :: k1) :: k2  -- F4 :: forall k1 k2. k1 -> k2
```

[](https://stackoverflow.com/a/20558716/412417)

https://stackoverflow.com/questions/22116363/what-is-the-purpose-of-data-proxy
Data.Tagged
Data.Proxy
https://stackoverflow.com/a/22116440/412417
https://en.wikibooks.org/wiki/Haskell/The_Curry%E2%80%93Howard_isomorphism
