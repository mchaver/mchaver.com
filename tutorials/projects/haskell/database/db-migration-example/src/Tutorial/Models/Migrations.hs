{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tutorial.Models.Migrations (
    migrateFromV1toV2
  , migrateFromV2toV3
  , migrateFromV3toV4
  , migrateFromV1toV4
  ) where

{-
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Logger (NoLoggingT, runNoLoggingT, MonadLogger)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceT)
-}

import           Control.Monad (forM_)
import           Database.Persist
import           Database.Persist.Sqlite

import qualified Tutorial.Models.Migrations.Models20160101 as M1
import qualified Tutorial.Models.Migrations.Models20160201 as M2
import qualified Tutorial.Models.Migrations.Models20160301 as M3
import qualified Tutorial.Models.Migrations.Models20160401 as M4


--  (MonadBaseControl IO m, MonadIO m)=> Text-> SqlPersistT (NoLoggingT (ResourceT m)) a-> m a
--  migrateFromV1toV2 ::  (MonadBaseControl IO m, MonadIO m)=> SqlPersistT (NoLoggingT (ResourceT m)) a-> m a

-- this assumes that the current db struct is set to

-- after you do runMigration, you might not be able to access the database with the
-- old interface

convertKey :: (ToBackendKey (PersistEntityBackend a) b, ToBackendKey (PersistEntityBackend a) a) => Entity a -> (Key b)
convertKey = fromBackendKey . toBackendKey . entityKey

-- add "password Text Maybe" column to User
migrateFromV1toV2 :: IO ()
migrateFromV1toV2 = runSqlite "test.sqlite" $ do
  usersV1 <- selectList ([] :: [Filter M1.User]) []
  runMigration M2.migrateAll
  -- we do not have to manually change anything because the column we added
  -- is a Maybe, it can automatically be added to existing instances of Users
  return ()


-- add "email Text" column to User
migrateFromV2toV3 :: IO ()
migrateFromV2toV3 = runSqlite "test.sqlite" $ do
  usersV2 <- selectList ([] :: [Filter M2.User]) []
  -- Yesod migration cannot forcibly insert a column unless it is [], Maybe
  -- or has a default tag
  -- there has been a column added to users that is required
  -- if are instances in SQLite when we try to migrate it will fail
  -- so we have to remove everything
  -- otherwise you can add the default tag to the end of the column declaration
  -- default=''
  -- default=0
  forM_ usersV2 $ \userV2 -> delete $ entityKey userV2

  runMigration M3.migrateAll

  -- reinsert the Users
  forM_ usersV2 $ \userV2 -> do
    let m3UserKey = convertKey userV2 :: (Key M3.User)
    insertKey m3UserKey (M3.User (M2.userIdent.entityVal $ userV2) (M2.userPassword.entityVal $ userV2) "")


-- remove "email Text" column from User
-- add new Entity Email
migrateFromV3toV4 :: IO ()
migrateFromV3toV4 = runSqlite "test.sqlite" $ do
  usersV3 <- selectList ([] :: [Filter M3.User]) []
  -- we are removing an existing column, data will be loss so we have to use
  -- runMigrationUnsafe
  runMigrationUnsafe M4.migrateAll
  forM_ usersV3 $ \userV3 -> do
    let m4UserKey = convertKey userV3 :: (Key M4.User)
    insert $ M4.Email (M3.userEmail.entityVal $ userV3) m4UserKey

-- if you rename a field, you have to manually move the data over
-- runMigration will not help you

-- if a data type is changed, runMigration will complain if it cannot
-- automatically convert the data type

-- http://www.yesodweb.com/book/persistent


-- insert some test data into V1 of the database
-- migrate through all the changes
migrateFromV1toV4 :: IO ()
migrateFromV1toV4 = do
  runSqlite "test.sqlite" $ do
    runMigration M1.migrateAll
    insert $ M1.User "James"
    insert $ M1.User "George"

  migrateFromV1toV2 >> migrateFromV2toV3 >> migrateFromV3toV4
  return ()
