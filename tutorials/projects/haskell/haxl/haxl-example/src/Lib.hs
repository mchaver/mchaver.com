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
  mUsers <- for userIds $ \userId -> do
    getUsernameById userId
  return $ catMaybes mUsers

getAllUserIds :: GenHaxl () [Key User]
getAllUserIds = dataFetch GetAllIds

getUsernameById :: (Key User) -> GenHaxl () (Maybe User)
getUsernameById userId = dataFetch (GetUserById userId)

insertUsers :: [User] -> GenHaxl () ()
insertUsers users = dataFetch (InsertUsers users)

-- type Haxl = GenHaxl ()

-- Data source implementation.

data SQLiteDBRequest a where
  GetAllIds   :: SQLiteDBRequest [Key User]
  GetUserById :: Key User -> SQLiteDBRequest (Maybe User)
  InsertUsers :: [User] -> SQLiteDBRequest ()
  deriving (Typeable)

deriving instance Eq (SQLiteDBRequest a)
instance Hashable (SQLiteDBRequest a) where
   hashWithSalt s GetAllIds       = hashWithSalt s (0::Int)
   hashWithSalt s (GetUserById a) = hashWithSalt s (1::Int, a)
   hashWithSalt s (InsertUsers a) = hashWithSalt s (2::Int, a)

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
        flip runSqlPersistMPool pool $ do
          forM_ users $ \user -> do
            mUser <- selectFirst [UserName ==. (userName user)] []
            case mUser of
              Nothing -> do
                _ <- insert user
                return ()
              Just _  -> return ()

{-
  fetch _state _flags _userEnv blockedFetches = SyncFetch $ do
    let pool = cp _state

    {-
    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser
    -}
    unless (null allIdVars) $
      flip runSqlPersistMPool pool $ do
        users <- selectList ([] :: [Filter User]) []
        let userKeys = entityKey <$> users
        liftIO $ mapM_ (\r -> putSuccess r userKeys) allIdVars

    unless (null userVars) $
      flip runSqlPersistMPool pool $ do
        mUsers <- for allIdVars $ \idVar -> do
          eUserId <- takeResult idVar
          case eUserId of
            Left _ -> return Nothing
            Right userId -> do
              mUser <- selectFirst [UserId ==. userId] []
              return $ entityVal <$> mUser
        --users <- selectList ([] :: [Filter User]) []
        --let userVals = entityVal <$> users
        liftIO $ mapM_ (\r -> putSuccess r mUsers) userVars

    where
      allIdVars :: [ResultVar [Key User]]
      allIdVars = [r | BlockedFetch GetAllIds r <- blockedFetches]

      userVars :: [ResultVar [Maybe User]]
      userVars = [r | BlockedFetch (GetUserById userId) r <- blockedFetches]
-}
