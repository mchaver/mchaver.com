{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Api where


import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe

import           Data.Text
import           Data.Typeable ( Proxy(Proxy) )

import           GHCJS.JSVal.Combinators
import           GHCJS.Marshal
-- import           GHCJS.Prim
-- import           GHCJS.Types

import           Servant.API
import           Servant.Client

import           Test.Api
import           Test.Types

instance ToJSVal User where
  toJSVal (User name age) = createObject [
      "name" .=> name
    , "age"  .=> age
    ]

instance FromJSVal User where
  fromJSVal v = runMaybeT $ User <$> v .-> "name"
                                 <*> v .-> "age"


data TestAPIInterface = TestAPIInterface {
  apiAddUser :: User -> EitherT ServantError IO ()
, apiGetUser :: Text -> EitherT ServantError IO (Maybe User)
}

createTestAPIInterface :: IO (TestAPIInterface)
createTestAPIInterface = do
  return $ TestAPIInterface apiAddUser' apiGetUser'
  where
    apiAddUser' :: User -> EitherT ServantError IO ()
    apiGetUser' :: Text -> EitherT ServantError IO (Maybe User)
    apiAddUser' :<|> apiGetUser' = client api $ Just $ BaseUrl scheme url port
    
    api :: Proxy TestAPI
    api = Proxy
    url = "127.0.0.1"
    port = 8081
    scheme = Http
