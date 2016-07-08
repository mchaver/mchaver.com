# db-migration-example

A Persistent package that has a set of migration functions based on
Persistent Models and changes to those Models are organized under a timestamp.

## File Structure

Each version of a package that changes a model file should move the model and its
dependencies (Typeclass instances and related data models and function) into a
new file or folder structure.

Model file in the first version, Version 0.1.0.0

```haskell
module Tutorial.Models.Migrations.Models20160101 where
...

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  ident Text
|]
```

In Version 0.1.1.0 there are no changes to the persist models. We do not need to
make any changes to the migrations or model files.

In Version 0.1.2.0 we make a change to the model file so we copy it over into a
new file and make changes there. We still have the old file as well.

```haskell
module Tutorial.Models.Migrations.Models20160201 where
...

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  ident Text
  password Text Maybe
|]
```

Now in the migration file we can access both models and support any changes between
the data types.

```haskell
module Tutorial.Models.Migrations where

import qualified Tutorial.Models.Migrations.Models20160101 as M1
import qualified Tutorial.Models.Migrations.Models20160201 as M2
```

## Persistent Migration Behavior

There are a few cases in which Persist will not automatically update the DB
schema.

- Add Field: if it is not nullable (Maybe or List) and does not have a `default` value
the migration will not update the schema if there are any instances in the database.
You have to delete all the instances of the Entity (Table) you are adding a field to and
reinsert it. Otherwise you can set the field as a Maybe, List or give it a default
value.
- Field (Column) or Entity (Table) renames: you need to manually move data from
the old column or entity to the new one.
- Field (Column) removal: if you want to remove an existing column you have to use
`runMigrationUnsafe` (data will be deleted).


[Persistent](http://www.yesodweb.com/book/persistent)

## Testing

You should test the expected behavior of all your migrations before using them in
a production environment. For example, if you add a field to `User` and you
have to delete and reinsert them all, it is a good idea to make sure you end up
with the same number of users. 
