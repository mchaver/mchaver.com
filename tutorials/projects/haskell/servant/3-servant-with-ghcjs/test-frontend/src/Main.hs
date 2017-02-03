{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Api
import           Control.Concurrent
import           Control.Monad.Trans.Either (runEitherT)
import           Test.Types

main :: IO ()
main = do 
  putStrLn "Hello, from GHCJS!"
  interface <- createTestAPIInterface
  ePostUser <- runEitherT $ apiAddUser interface (User "newUser" 80)
  case ePostUser of
    Left _ -> do
      print "There was an error uploading the user."
    Right _ -> do
      -- threadDelay (1000000 * 3)
      eUser <- runEitherT $ apiGetUser interface "newUser"
      case eUser of
        Left _ -> do
          print "Failed to receive the user. Make sure that the servant server is running and the port is set correctly."
        Right user -> do
          print "Received the user from the server:"
          print user


{-
eitherUsers <- runEitherT $ apiGetUsers interface
  case eitherUsers of
    Left _ -> return ()
    Right users -> do
      liftIO $ print "users succesfully returned"
      runMessages $ sendMessage (snd usersMb) $ S.fromList users
      getAuditViews interface (_getUIIUserId <$> users)
      liftIO $ print users
  
data TestAPIInterface = TestAPIInterface {
  apiAddUser :: User -> EitherT ServantError IO ()
, apiGetUser :: Text -> EitherT ServantError IO (Maybe User)
}

-}
