---
title: Introducing simple-store and simple-cell
tags: haskell, db
---

simple-store[^1] provides persistence, atomicity and consistency for a shared data 
type. simple-cell[^2] uses simple-store and directed-keys[^3] to create multiple 
atomic values with the unique keys. We will review a few concepts before 
discussing these packages further.

## Review

#### Concurrency control

Concurrency control[^4] ensures that concurrent operations are safe and fast and 
solves the following problems:

- lost update
- dirty read
- incorrect summary

We will only discuss one type here, optimistic concurrency control.

#### Optimistic concurrency control (OCC)

OCC[^5] assumes that multiple transactions can be made frequently without 
interfering with each other. OCC does not use locks[^6]. Rather, before committing 
each transaction, it verifies that no other transaction has modified the data 
it has read. If the check reveals that modifications have occurred, the 
committing transaction rolls back and can be restarted. OCC is good for 
environments with low data contention. 

#### OCC phases

- begin: record a timestamp which marks the beginning of a transaction.
- modify: read db values, tentatively write changes.
- validate: check if other transactions have modified the data that the transaction
has read/wrote.
- commit/rollback: if there is no conflict, make all changes take effect, 
otherwise abort the changes.

#### Software transactional memory (STM)

STM[^7] is a type of OCC. A thread modifies shared memory without concern for what 
other threads may be doing to that memory. STM makes the reader responsible for 
making sure nothing is operating on the shared memory.

#### Concurrent Haskell and STM

#### MVar

`MVar t`[^8] is a mutable location that is empty or contains a value `t`. 

- `putMVar` sets the value in an `MVar`.
- `readMVar` gets the value in an `MVar` and sets it to the value it just took.
- `takeMVar` gets the value in an `MVar` and sets the value to empty.

#### TMVar

`TMVar t`[^9] is the STM version of `MVar` and is thread safe.

- `putTMVar` sets the value in an `TMVar`.
- `readTMVar` gets the value in an `TMVar` and sets it to the value it just took.
- `takeTMVar` gets the value in an `TMVar` and sets the value to empty.

## Dependencies

#### cereal

cereal[^10] is a package the performs binary serialization. By declaring an instance 
of the `Serialize` type class, we can perform serialization and deserialization
on a type. It can also use `GHC.Generics` to automatically declare a `Serialize`
instance.

#### directed-keys

directed-keys provides a data type and functions to serialize data to and from 
Base64[^11]. 

- `DirectedKeyRaw` is data type that  has a unique key, a source url/file path, a destination 
url/file path and a time. They all need an instance of `Hashable` and 
`Serialize`.
- `DirectedKey` is a `ByteString` of the data in `DirectedKeyRaw`.
- `encodeKeyRaw` is like `DKeyRaw` constructor, but it enforces the `Serialize` 
restriction on the keys.
- `encodeKey` and `decodeKey` to an from a base64 `ByteString`.
- `parseFilename` and `decodeFilename` escape and unescape the necesssary Unix 
characters in a file path.

#### simple-store

The main data type is `SimpleStore`. You do not need to manipulate `SimpleStore`
records directly. simple-store provides a set of functions to save and retrieve 
data via the `SimpleStore` data type and filesystem. 

```haskell
data SimpleStore st = 
  SimpleStore
    { storeFP     :: FilePath
    , storeState  :: TVar st
    , storeLock   :: TMVar StoreLock
    , storeHandle :: Maybe Handle
    }
```

The most important functions are:

- `makeSimpleStore` save a serializable type to a file
- `openSimpleStore` read a deserializable type from a file
- `getSimpleStore` get the data type value from the `SimpleStore`.
- `modifySimpleStore`
- `updateSimpleStore`

#### simple-cell

A `SimpleCell` takes a function to retrieve keys and a function to make those 
keys into filenames. It maintains a key-value pair of filename to `SimpleStore`.
By convention, simple-cell uses a type suffixed `Store` as a newtype wrapped 
entity of an entity with DB specific properties (like external keys).

```haskell
newtype User = User 
  { name :: String 
  }

data UserStore = UserStore 
  { userKey   :: !Int
  , userValue :: !Maybe User
  } 
  deriving (Eq,Generic,Show)
```

The general work flow is:

- Declare CellKey value for the data type we want to store.
- Generate functions with Template Haskell.
- Initialize a `SimpleStore`.
- Pass the cell around manually or with `ReaderT`.
- Insert data with `insert<Type>SC`.
- Perform batch operations with `foldlWithKey<Type>SC`
- Get stores out the cell with `get<Type>SC`. 

We need to define three functions for `CellKey` for looking up, decoding and 
encoding a `DirectedKeyRaw`. 

```haskell
data CellKey k src dst tm st = CellKey 
  { getKey                :: st -> DirectedKeyRaw k src dst tm
  , codeCellKeyFilename   :: DirectedKeyRaw k src dst tm -> Text
  , decodeCellKeyFilename :: (Text -> Either Text (DirectedKeyRaw k src dst tm)) 
  }
```

Then we use Template Haskell to produce a set of type specific functions.

```haskell
$(makeStoreCell 'userStoreCellKey 'initUser ''User)
```

`makeStoreCell` generates the following functions, but you have to provide 
the type signature for each of them to help the Template Haskell.

- `get<Type>SC`
- `update<Type>SC`
- `createCheckpointAndClose<Type>SC`
- `traverseWithKey<Type>SC_`
- `insert<Type>SC`
- `delete<Type>SC`
- `foldlWithKey<Type>SC`
- `initialize<Type>SC`

We generally do not need to manipulate the `SimpleCell` data type directly, but 
it is helpful to know what it contains.

```haskell
data SimpleCell  k src dst tm stlive stdormant = SimpleCell {
  { cellCore     :: !(TCellCore  k src dst tm (SimpleStore stlive) stdormant )
  , cellKey      :: !(CellKey  k src dst tm stlive)
  , cellParentFP :: !FilePath
  , cellRootFP   :: !FilePath
  } deriving (Typeable,Generic)
```

- `cellCore` is an in-memory representation of the map of keys and `SimpleStore`s.
- `cellKey` is the key provided by the user.
- `cellParentFP` is the file path that the root of the project is in.
- `cellRootFP` is the file path that the cell occupies.

Types to remember for simple-cell:

- `DirectedKeyRaw`
- `CellKey`
- `SimpleCell`
- `SimpleStore`

For a complete example, take a look at the [simple-cell tests](https://github.com/plow-technologies/simple-cell/blob/master/test/TestImport.hs).


## References

[^1]: [Github :: simple-store](https://github.com/plow-technologies/simple-store)

[^2]: [Github :: simple-cell](https://github.com/plow-technologies/simple-cell)

[^3]: [Github :: directed-keys](https://github.com/plow-technologies/directed-keys)

[^4]: [Wikipedia :: Concurrency control](https://en.wikipedia.org/wiki/Concurrency_control)

[^5]: [Wikipedia :: Optimistic concurrency control](https://en.wikipedia.org/wiki/Optimistic_concurrency_control)

[^6]: [Wikipedia :: Lock (computer science)](https://en.wikipedia.org/wiki/Lock_(computer_science))

[^7]: [Real World Haskell :: Software transactional memory](http://book.realworldhaskell.org/read/software-transactional-memory.html)

[^8]: [Hackage :: stm :: MVar](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Concurrent-MVar.html)

[^9]: [Hackage :: stm :: TMVar](https://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Concurrent-STM-TMVar.html)

[^10]: [Hackage :: cereal](https://hackage.haskell.org/package/cereal-0.5.4.0/docs/Data-Serialize.html)

[^11]: [Wikipedia :: Base64](https://en.wikipedia.org/wiki/Base64)
