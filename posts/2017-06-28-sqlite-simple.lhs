---
title: Simple SQLite queries with sqlite-simple
---

== Motivation

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
possible SQL queries. However, they also remove the burden of writing 
error-prone boilerplate code and provide raw query options.

sqlite-simple[^4] allows us to write simple type class serializations for data 
types, `ToRow` and `FromRow`, and raw database queries.

== Example

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (mapM_)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok

data Person =
  Person 
    { personId   :: Int64
    , personName :: Text
    , personAge  :: Text
    } deriving (Eq,Read,Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

-- when inserting a new Person, ignore personId. SQLite will provide it for us.
instance ToRow Person where
  toRow (Person _pId pName pAge) = toRow (pAge, pName)

runPersonExample :: IO ()
runPersonExample = do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age TEXT)"
  execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Justina" "15")
  execute conn "INSERT INTO people (name, age) VALUES (?,?)" (Person 0 "Jordi" "11")
  people <- query_ conn "SELECT id, name, age from people" :: IO [Person]
  close conn
  print people

\end{code}

Now we will define a new field and new row type and use a join.

\begin{code}
data PhoneType = HomePhone | CellPhone | WorkPhone
  deriving (Eq,Read,Show)

instance ToField PhoneType where
  toField = SQLText . T.pack . show
  
instance FromField PhoneType where
  fromField (Field (SQLText "HomePhone") _) = Ok HomePhone
  fromField (Field (SQLText "CellPhone") _) = Ok CellPhone
  fromField (Field (SQLText "WorkPhone") _) = Ok WorkPhone
  fromField f = returnError ConversionFailed f "need 'HomePhone', 'CellPhone' or 'WorkPhone'"

data Phone =
  Phone 
    { phoneId       :: Int64
    , phoneNumber   :: Text
    , phoneType     :: PhoneType
    , phonePersonId :: Int64
    } deriving (Eq,Read,Show)

instance FromRow Phone where
  fromRow = Phone <$> field <*> field <*> field <*> field

-- when inserting a new Phone, ignore phoneId. SQLite will provide it for us.
instance ToRow Phone where
  toRow (Phone _pId pNumber pType pPersonId) = toRow (pNumber, pType, pPersonId)

data PeoplePhonePair =
  PeoplePhonePair
    { ppPersonId    :: Int64
    , ppName        :: Text
    , ppAge         :: Text
    , ppPhoneNumber :: Text
    , ppPhoneType   :: PhoneType
    } deriving (Eq,Read,Show)

instance FromRow PeoplePhonePair where
  fromRow = PeoplePhonePair <$> field <*> field <*> field <*> field <*> field

runPhoneExample :: IO ()
runPhoneExample = do
  conn     <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS person (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT, age TEXT)"
  execute_ conn "CREATE TABLE IF NOT EXISTS phone (id INTEGER PRIMARY KEY AUTOINCREMENT, number TEXT, type TEXT, person_id INTEGER)"
  
  execute conn "INSERT INTO person (name, age) VALUES (?,?)" (Person 0 "Justina" "15")
  justinaId <- lastInsertRowId conn
  print justinaId
  
  execute conn "INSERT INTO person (name, age) VALUES (?,?)" (Person 0 "Jordi" "11")
  jordiId <- lastInsertRowId conn
  print jordiId
  
  print "people"
  people <- query_ conn "SELECT id, name, age from person" :: IO [Person]
  
  print people

  justina <- queryNamed conn "SELECT id, name, age from person where id = :id" [":id" := justinaId] :: IO [Person]
  print justina
  jordi   <- queryNamed conn "SELECT id, name, age from person where id = :id" [":id" := jordiId]   :: IO [Person]
  print jordi
  
  execute conn "INSERT INTO phone (number, type, person_id) VALUES (?,?,?)" (Phone 0 "123456789" HomePhone justinaId)
  execute conn "INSERT INTO phone (number, type, person_id) VALUES (?,?,?)" (Phone 0 "111222333" CellPhone justinaId)
  execute conn "INSERT INTO phone (number, type, person_id) VALUES (?,?,?)" (Phone 0 "987654321" CellPhone jordiId)
  execute conn "INSERT INTO phone (number, type, person_id) VALUES (?,?,?)" (Phone 0 "444555666" WorkPhone jordiId)
  
  peoplePhonePairs <- query_ conn "SELECT person.id, name, age, number, type from person LEFT OUTER JOIN phone ON person.id = phone.person_id" :: IO [PeoplePhonePair]
  
  mapM_ print peoplePhonePairs
  close conn


main :: IO ()
main = runPhoneExample
\end{code}

== Conclusion

I think this highlights the repetition that is required to create a full 
application and motivates the use of libraries like persistent and groundhog.
If you only need some simple queries, then sqlite-simple is sufficient. 
Otherwise the burden of manually writing boilerplate non-type safe queries may 
be too high.

Moreover, all of these queries are unsafe IO. If `execute` or `query` fail then an 
exception will be raised. It would probably be a good idea to extend `sqlite-simple`
to `executeMay` and `queryMay` that return `IO (Either SQLError a)`. That way 
we are required to program type safe behavior.


[^1]: [Hackage :: persistent](https://hackage.haskell.org/package/persistent)
[^2]: [Hackage :: groundhog](https://hackage.haskell.org/package/groundhog)
[^3]: [Hackage :: esqueleto](https://hackage.haskell.org/package/esqueleto)
[^4]: [Hackage :: sqlite-simple](https://hackage.haskell.org/package/sqlite-simple)
