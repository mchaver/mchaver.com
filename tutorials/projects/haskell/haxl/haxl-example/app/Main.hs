{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad.Logger (runStderrLoggingT)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Haxl.Core

import           Lib

import           Models

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createSqlitePool ":memory:" 1
  runSqlPool (runMigration migrateAll) pool

  let stateStore = stateSet ( SQLiteDBState pool 1 ) stateEmpty

  -- initEnv creates an empty cache
  env <- initEnv stateStore ()

  print "Insert a single user"
  runHaxl env $ insertUsers [ User "孔子" 73 ]
  print "Run select query, should perform SQL select once"
  names <- runHaxl env getAllUsers
  print names

  print "Insert 7 users"
  runHaxl env $ insertUsers [ User "孟子" 83 ]
  runHaxl env $ insertUsers [ User "莊子" 83
                            , User "老子" 100
                            , User "荀子" 75
                            , User "墨子" 92
                            , User "韓非子" 99
                            , User "朱熹" 69 ]
  print "Run 3 select queries, should only perform one set of SQL select once"

  -- the cache is not working properly, does not perform a new sql query
  -- if env is inited again it works but that is because it starts with an empty cache
  -- env <- initEnv stateStore ()

  names2 <- runHaxl env getAllUsers
  print names2

  names3 <- runHaxl env getAllUserIds
  print names3
  {-
  names4 <- runHaxl env getAllUsers
  print names4
  -}
