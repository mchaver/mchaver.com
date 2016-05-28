
import qualified Data.HashMap.Strict as H

H.member

data Where = Here | There deriving Show

version before aeson-0.10.0.0
0.11.2.0

can handle values at the top level

decode
encode

Remote procedure calling
Foreign Function Interface

should build tests for intended behavior

If you only have a few JSONs it is easy to keep track of, but as it grows it is more challenging

If you want better control over the value naming as it appears in the JSON write you own `ToJson` and how it is marshalled fromJSON to haskell then implement `FromJSON`.

JSON is fragile. Best to derive your own instances.

http://hackage.haskell.org/package/aeson-0.11.2.0/docs/Data-Aeson.html

type Object = HashMap Text Value
type Array = Vector Value
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null

Product

To be able to parse sum type (fromJSON). Each data type either needs to have a unique key name or a unique value in that key. An easy way to handle this is treat the constructor as a key because these are necessarily unique.

Parsing data of varying structure in Haskell with Aeson
http://bitemyapp.com/posts/2014-04-17-parsing-nondeterministic-data-with-aeson-and-sum-types.html

Algebraic Data Types (ADTs) with Aeson
https://www.schoolofhaskell.com/user/Geraldus/algebraic-data-types-adts-with-aeson