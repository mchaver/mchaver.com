{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)

import           Data.String.Conversions
import           Data.Text

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Network.Wai
import qualified Network.Wai.Handler.Warp as Warp

import           Servant

import           Test.Api
import           Test.Models

runDB :: ConnectionPool -> SqlPersistT IO a -> IO a
runDB pool query = liftIO $ runSqlPool query pool


server :: ConnectionPool -> Server TestAPI
server pool =
  userAddH :<|> userGetH
  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name    = liftIO $ userGet name

    userAdd :: User -> IO (Maybe UserId)
    userAdd newUser = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [UserName ==. (userName newUser)] []
      case exists of
        Nothing -> Just <$> insert newUser
        Just _ -> return Nothing

    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser

testAPI :: Proxy TestAPI
testAPI = Proxy

app :: ConnectionPool -> Application
app pool = serve testAPI $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool

runApp :: FilePath -> IO ()
runApp sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile

main :: IO ()
main = runApp "test.sqlite"
