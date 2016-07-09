module Main where

import Tutorial.Models.Migrations

main :: IO ()
main = do
  migrateFromV1toV4
