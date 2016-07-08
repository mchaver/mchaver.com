{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.Trans.Except

import Data.Proxy
import Data.Text

import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import Servant.API
import Servant.Client

import Test.Api
import Test.Models


testAPI :: Proxy TestAPI
testAPI = Proxy

userAdd :: User -> Manager -> BaseUrl -> ExceptT ServantError IO (Maybe UserId)
userGet :: Text -> Manager -> BaseUrl -> ExceptT ServantError IO (Maybe User)

userAdd :<|> userGet = client testAPI

queries :: Manager -> BaseUrl -> ExceptT ServantError IO (Maybe User)
queries manager baseurl = do
  _ <- userAdd (User "Alice" 26) manager baseurl
  userGet "Alice" manager baseurl

main :: IO ()
main = do
  putStrLn "Running Test Client"
  manager <- newManager defaultManagerSettings
  res <- runExceptT (queries manager (BaseUrl Http "localhost" 3000 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right user -> print user
