{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Api where

import Data.Aeson
import Data.Text

import Database.Persist

import GHC.Generics

import Servant.API

import Test.Types


instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "name" 
         <*> v .: "age"

instance ToJSON User where
  toJSON (User userName userAge) = 
    object [ "name" .= userName
           , "age"  .= userAge  ]  

type TestAPI = "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] ()
          :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)

