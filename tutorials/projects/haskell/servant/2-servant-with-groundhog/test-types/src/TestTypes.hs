{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module TestTypes where

import Control.Monad.IO.Class (liftIO)

import Data.Text (Text)

import Database.Groundhog.Sqlite
import Database.Groundhog.TH

data User = User {
  userName :: Text
, userAge  :: Int
} deriving Show

data Friend = Friend {
  fstPerson :: DefaultKey User
, sndPerson :: DefaultKey User
}

deriving instance Show Friend


mkPersist defaultCodegenConfig [groundhog|
- entity: User               # Name of the datatype
  constructors:
    - name: User
      fields:
        - name: userName
          # Set column name to "name" instead of "customerName"
          dbName: name
      uniques:
        - name: NameConstraint
          fields: [userName] # Inline format of list
- entity: Friend
|]

{-
-- show

-- /show
-}
