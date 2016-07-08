{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import Data.Text

data User = User {
  name :: Text
, age  :: Int
} deriving (Eq,Read,Show)

instance ToJSON User where
  toJSON (User name age) = object ["name" .= name, "age" .= age]
  
instance FromJSON User where
  parseJSON (Object v) = User <$> v .: "name" <*> v .: "age" 
