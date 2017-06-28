---
title: Simple SQLite queries with sqlite-simple
---

## Motivation

Haskell libraries like persistent[^1] and groundhog[^2] offer feature complex 
database connections. However, these libraries use Template Haskell to produce a 
large amount of types and type classes to simplify serialization to and from 
the database. This increases compile times and introduces more complexity from 
the perspective of the developer.

persistent is very opinionated about how you structure your database. This makes 
it difficult to work with databases that do not fit persistent's expected 
structure. groundhog is more flexible in terms of how your types are serialized 
to and from a database. While esqueleto[^3] extends persistent to permit joins, 
groundhog does not support joins and both libraries are limited to a subset of
possible SQL queries.

sqlite-simple[^4] allows us to write simple type class serializations for data 
types, `ToRow` and `FromRow`, and raw database queries.

## Example

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text (Text)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data Person =
  Person 
    { personId   :: Int
    , personName :: Text
    , personAge  :: Int 
    } deriving (Eq,Read,Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

-- when inserting a new Person, ignore the id. SQLite will provide it for us
instance ToRow Person where
  toRow (Person _pId pName pAge) = toRow (pAge, pName)

main :: IO ()
main = do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age INT)"
  execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Justina" 15)
  execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Jordi" 11)
  people <- query_ conn "SELECT id, name, age from people" :: IO [Person]
  close conn
  print people
```

[^1]: [Hackage :: persistent](https://hackage.haskell.org/package/persistent)
[^2]: [Hackage :: groundhog](https://hackage.haskell.org/package/groundhog)
[^3]: [Hackage :: esqueleto](https://hackage.haskell.org/package/esqueleto)
[^4]: [Hackage :: sqlite-simple](https://hackage.haskell.org/package/sqlite-simple)
