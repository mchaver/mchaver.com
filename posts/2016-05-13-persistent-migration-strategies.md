---
title: Persistent Migration Strategies
---

The Persistent Haskell package does not provide many options for migration strategies. You can easily add new tables or add rows with a `Maybe a` type or `[a]` type to an existing table without breaking the database. However, if you want to remove rows, add rows with `Maybe` or `[]`, or change row types then you need to migrate the data. I will share a few strategies I have for dealing with these sort of changes. 


```
User
  ident Text
  
```
## Use `Maybe` to Avoid Migrations

At the database level you do not have to make any changes. However, in your code you will have to handle the new rows appropriately. `Maybe` is useful for cases where the row may actually be `Nothing`. If that is the semantic significance you want to give to that row, then go ahead and use it. However, it is a bad idea to use `Maybe` as a band-aid to avoid making migration functions because your model contains a type that misrepresents your intention. It makes the meaning more opaque and your models less robust. It may confuse people whom work together with you and yourself at a later time. 


## Separate Model Changes

use `qualified` to import models
each migration has its own database

## Handling Type Changes that use `derivePersistField`

-- derivePersistField "AuditAction"
because of Template Haskell they have to be defined in a separate file.
Any changes to a data type that uses `derivePersistField` should be considered a migration. Manage declarations of `derivePersistField` the same way that models are managed.


## Define Migration Functions

## Link functions for multi level migration

## One model file per version

Between versions you do not need to have a separate model file for each change unless you are experimenting. Also, if there are no changes to the model file then you do not need to create a new one between versions. 

For example, our first version is 0.1.0.0, we have one model file. After commiting and releasing to production, we decide we need to make changes to the database, we create one new model file and accompanying migrations. This file might have a few changes before releasing the next version 0.2.0.0. Once it we release 0.2.0.0 we should no longer change this file. The next time we make changes to the structure of the database, we need to create a new model file. In our next set of updates from 0.2.0.0 to 0.3.0.0 we do not change any of the models and do not need a migration so we do not create a new model file.

## Create Migration Tests

Create expectations, when moving from one schema to another do you expect to that all of the data is moved over? Compare the entries in the old database and new one to make sure they are all there, comparing on unique elements should help. Should some items not be added? 

## Model File Naming

Version number or timestamp. I personally prefer version number.

## Supporting different versions with different schemas

If you have an versioned API, you may have to maintain multiple versions of your database. If a user moves from one API to another their data may migrate as well. This could possibly get quite complicated so it is important to prove expected behavior with testing.


[Catalog of Database Refactorings](http://www.agiledata.org/essays/databaseRefactoringCatalog.html)