{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

-- Incidental:
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Hashable
import           Data.List hiding (insert)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Data.Typeable

import           Database.Persist.Sql

import           Haxl.Core hiding (try)

import           Models

import           System.Random


-- Data source API.

getAllUsers :: GenHaxl () [User]
getAllUsers = do
  userIds <- getAllUserIds
  mUsers <- for userIds $ \userId ->
    getUsernameById userId
  return $ catMaybes mUsers

getAllUserIds :: GenHaxl () [UserId]
getAllUserIds = dataFetch GetAllIds

getUsernameById :: UserId -> GenHaxl () (Maybe User)
getUsernameById userId = dataFetch (GetUserById userId)

insertUsers :: [User] -> GenHaxl () ()
insertUsers users = dataFetch (InsertUsers users)

-- Data source implementation.

data SQLiteDBRequest a where
  GetAllIds   :: SQLiteDBRequest [UserId]
  GetUserById :: Key User -> SQLiteDBRequest (Maybe User)
  InsertUsers :: [User] -> SQLiteDBRequest ()
  deriving (Typeable)

deriving instance Eq (SQLiteDBRequest a)
instance Hashable (SQLiteDBRequest a) where
   hashWithSalt s r@(GetAllIds)     = hashWithSalt s (0::Int, show r)
   hashWithSalt s r@(GetUserById _) = hashWithSalt s (1::Int, show r)
   hashWithSalt s r@(InsertUsers _) = hashWithSalt s (2::Int, show r)

deriving instance Show (SQLiteDBRequest a)
instance Show1 SQLiteDBRequest where show1 = show

instance StateKey SQLiteDBRequest where
  data State SQLiteDBRequest =
    SQLiteDBState
      { sqliteDBStateConnectionPool :: ConnectionPool
      , sqliteDBStateNumThreads     :: Int
      }

instance DataSourceName SQLiteDBRequest where
  dataSourceName _ = "SQLiteDBRequest"

instance DataSource u SQLiteDBRequest where
  fetch = sqliteDBFetch

{- Synchronous version
sqliteDBFetch :: State SQLiteDBRequest
              -> Flags
              -> u
              -> [BlockedFetch SQLiteDBRequest]
              -> PerformFetch
sqliteDBFetch SQLiteDBState{..} _ _ blockedFetches =
  SyncFetch $ mapM_ (sqliteDBFetchSync sqliteDBStateConnectionPool) blockedFetches

sqliteDBFetchSync :: ConnectionPool -> BlockedFetch SQLiteDBRequest -> IO ()
sqliteDBFetchSync connectionPool (BlockedFetch req rvar) = do
  eitherExceptionResponse <- try $ fetchSQLiteDBReq connectionPool req
  case eitherExceptionResponse of
    Left exception -> putFailure rvar (exception :: SomeException)
    Right response -> putSuccess rvar response
-}

-- Asynchronous version
sqliteDBFetch :: State SQLiteDBRequest
              -> Flags
              -> u
              -> [BlockedFetch SQLiteDBRequest]
              -> PerformFetch
sqliteDBFetch SQLiteDBState{..} _ _ blockedFetches =
  AsyncFetch $ \inner -> do
    sem <- newQSem sqliteDBStateNumThreads
    asyncs <- mapM (sqliteDBFetchAsync sqliteDBStateConnectionPool sem) blockedFetches
    inner
    mapM_ wait asyncs

sqliteDBFetchAsync :: ConnectionPool -> QSem -> BlockedFetch SQLiteDBRequest -> IO (Async ())
sqliteDBFetchAsync connectionPool sem (BlockedFetch req rvar) =
  async $ bracket_ (waitQSem sem) (signalQSem sem) $ do
    eitherExceptionResponse <- try $ fetchSQLiteDBReq connectionPool req
    case eitherExceptionResponse of
      Left exception -> putFailure rvar (exception :: SomeException)
      Right response -> putSuccess rvar response

fetchSQLiteDBReq :: MonadIO m => ConnectionPool -> SQLiteDBRequest a -> m a
fetchSQLiteDBReq pool sqliteDBRequest =
  liftIO $
    case sqliteDBRequest of
      GetAllIds ->
        flip runSqlPersistMPool pool $ do
          users <- selectList ([] :: [Filter User]) []
          let userKeys = entityKey <$> users
          return userKeys
      GetUserById userId ->
        flip runSqlPersistMPool pool $ do
          user <- selectFirst [UserId ==. userId] []
          return $ entityVal <$> user
      InsertUsers users ->
        flip runSqlPersistMPool pool $
          forM_ users $ \user -> do
            mUser <- selectFirst [UserName ==. userName user] []
            case mUser of
              Nothing -> do
                _ <- insert user
                return ()
              Just _  -> return ()
