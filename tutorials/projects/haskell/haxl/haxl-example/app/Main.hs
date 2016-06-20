{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Logger (runStderrLoggingT)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite

import           Haxl.Core

import           Lib

import           Models

main :: IO ()
main = do
  pool <- runStderrLoggingT $ createSqlitePool "test.sqlite" 5
  runSqlPool (runMigration migrateAll) pool

  let stateStore = stateSet ( SQLiteDBState pool 2 ) stateEmpty
  env <- initEnv stateStore ()
  runHaxl env $ insertUsers [ User "Confucius" 70
                            , User "Mencius" 80
                            , User "Laozi" 66
                            , User "Zhuangzi" 99]
  names <- runHaxl env getAllUserIds
  runHaxl env $ insertUsers [ User "New Guy" 18 ]
  names2 <- runHaxl env getAllUserIds
  names3 <- runHaxl env getAllUserIds
  names4 <- runHaxl env getAllUserIds

  print names
  print names2
  print names3
  print names4
