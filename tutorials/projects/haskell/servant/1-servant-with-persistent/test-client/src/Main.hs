{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
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
import Test.Types


testAPI :: Proxy TestAPI
testAPI = Proxy


userAdd   :: User -> Manager -> BaseUrl -> ExceptT ServantError IO ()
userGet :: Text -> Manager -> BaseUrl -> ExceptT ServantError IO (Maybe User) 

userAdd :<|> userGet = client testAPI

queries :: Manager -> BaseUrl -> ExceptT ServantError IO (Maybe User)
queries manager baseurl = do
  userAdd (User "Alp" 26) manager baseurl
  mUser <- userGet "Alp" manager baseurl
  return mUser

main :: IO ()
main = do 
  putStrLn "Running Test Client"
  manager <- newManager defaultManagerSettings
  res <- runExceptT (queries manager (BaseUrl Http "localhost" 8081 ""))
  case res of 
    Left err -> putStrLn $ "Error: " ++ show err
    Right user -> print user
