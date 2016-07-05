{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Test.Api where

import Data.Text
import Servant.API
import Test.Models

type TestAPI = "user" :> "add" :> ReqBody '[JSON] User :> Post '[JSON] (Maybe UserId)
          :<|> "user" :> "get" :> Capture "name" Text  :> Get  '[JSON] (Maybe User)
