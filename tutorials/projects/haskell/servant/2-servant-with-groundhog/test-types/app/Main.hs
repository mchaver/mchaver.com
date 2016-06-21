{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)
import Database.Groundhog
import Database.Groundhog.TH
import Database.Groundhog.Sqlite

import TestTypes

main :: IO ()
main = withSqliteConn ":memory:" $ runDbConn $ do
  runMigration $ do
    migrate (undefined :: User)
    migrate (undefined :: Friend)
  johnKey <- insert $ User "John Doe" 35
  bobKey  <- insert $ User "Bob Ross" 25
  friendKey <- insert $ Friend johnKey bobKey

  get johnKey >>= liftIO . print
  get bobKey >>= liftIO . print
  get friendKey >>= liftIO . print

  liftIO $ print "Finished"
  {-
  insert $ Product "Oranges" 3 johnKey
  insert $ Product "Apples" 5 johnKey
  janeKey <- insert $ Customer "Jane Doe" "9876543210"
  insert $ Product "Oranges" 4 janeKey
  johnOrders <- select $ (CustomerField ==. johnKey)
    `orderBy` [Asc ProductNameField]
  liftIO $ putStrLn $ "Products for John: " ++ show johnOrders
  -}
