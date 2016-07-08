{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Models where

import Data.Aeson
import Data.Text

import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  age  Int
  UniqueName name
  deriving Eq Read Show
|]

{-|
the persist QuasiQuoter automatically produces a number of types and functions
the following are available:

Key User = PersistentKey SqlKey
UserId   = PersistentKey SqlKey
UserId and (Key User) are the same thing

User {userName :: Text, userAge :: Int}
UniqueName Text -> Filter

userName :: User -> Text
userAge  :: User -> Int
entityVal :: Entity User -> User
entityKey :: Entity User -> Key User
|-}

instance FromJSON User where
  parseJSON = withObject "User" $ \ v ->
    User <$> v .: "name"
         <*> v .: "age"

instance ToJSON User where
  toJSON (User usrName usrAge) =
    object [ "name" .= usrName
           , "age"  .= usrAge  ]
