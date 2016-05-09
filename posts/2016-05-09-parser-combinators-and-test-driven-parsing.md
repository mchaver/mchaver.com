---
title: Parser Combinators and Test Driven Parsing
---

[Attoparsec](http://hackage.haskell.org/package/attoparsec-0.13.0.2) is a Haskell parser combinator library for parsing `ByteString` and `Text`. We can use it to make small parsers and combine them in various ways. The advantage of this style is we can individually test each parser, making it easier to debug and build, as well as making the individual pieces reusable.

We are going to focus on parsing `Text` from `Data.Text`. The top of our parsing file looks like this.
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.Text
import Data.Text (Text)

```

We want string literals to be treated as `Text`, we want the attoparsec parser for `Text` and we want the `Text` type. Here is our first parser.

```haskell
parseHelloWorld :: Parser ()
parseHelloWorld = do
  _ <- string "Hello World!"
  return ()
```

It takes a `Text`, though you cannot tell that just by at the type signature (we will explore the Parser type in a later lesson) because it is a type synonym, and returns an `()`. Parsing returns an `Either Text a`, where `Text` is an error message and `a` is whatever we decide to return. In this case it is `()`. `string` takes a `Text` and returns it in a `Parser`. In this case we are ignoring it.

Let's make a test so that we can prove that it works. We will use two packages to help us with testing. `hspec` helps automatically find files for testing and provides some basic functions. You can test `attoparsec` with just `hspec`, but , `hspec-attoparsec` provides some convenient functions.

```haskell
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (main, spec) where

import Data.Text (Text)
import Parser

import Test.Hspec
import Test.Hspec.Attoparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "should parse the phrase 'Hello World!'" $
      parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)
```

You can run this from the command line with `cabal test`, and you should see all tests pass. `shouldSucceedOn` runs a parser from the left hand side with the input on the right hand side and allows the test to pass if the parser returns `Right`. Do you want to see what a failed test looks like? Just change the last line to:

```haskell
      parseHelloWorld `shouldSucceedOn` ("Hello World" :: Text)
``` 

You will see a "not enough input" error. The `string` function could not find the last character `!`. Let's slightly change the `parseHelloWorld` function so we can show off some more functionallity.

```haskell
parseHelloWorld2 :: Parser Text
parseHelloWorld2 = do
  result <- string "Hello World!"
  return result
```

Now the parse will return the result from the `string` function. We can verify this with another test.

```haskell
    it "should parse the phrase 'Hello World!' and return 'Hello World!'" $
      ("Hello World!" :: Text) ~> parseHelloWorld2 `shouldParse` ("Hello World!" :: Text)
```

The `(~>)` function feeds a `Text` to be parsed by `parseHelloWorld2` from the left hand side and `shouldParse` says not only should the parser succeed, but its result should match the right hand side. There are two ways this can fail. `parseHelloWorld2` could fail to parse the left hand side, or it could succeed in parsing it but it does not return the expectation on the right hand side. Try deleting something from the left or right hand side and see what happens when you run the test.

Sometimes we want to be able to prove that a parser will fail, just use `shouldFailOn`.

```haskell
    it "should fail to parse any other string" $ do
      parseHelloWorld `shouldFailOn` ("Goodnight Everyone!" :: Text)      
```

For failed cases we do not need to inspect the returned result because we just get an error message. Finally, let's see what a parser combinator looks like. We will split up the original function and glue it together in another one.

```haskell
parseHello :: Parser Text
parseHello = string "Hello"

parseWorld :: Parser Text
parseWorld = string " World!"

parseHelloWorld3 :: Parser Text
parseHelloWorld3 = do
  hello <- parseHello
  world <- parseWorld
  return $ T.concat [hello,world]
```

Add a new test for it.

```haskell
  describe "A Parser Combinator" $ do
    it "shoul parse 'Hello World!'" $
      ("Hello World!" :: Text) ~> parseHelloWorld3 `shouldParse` ("Hello World!" :: Text)
```

Try making your own parser combinator with various `string` functions.

References:

[Data.Attoparsec.Text function reference](http://hackage.haskell.org/package/attoparsec-0.13.0.2/docs/Data-Attoparsec-Text.html)

[hspec-attoparsec reference](http://hackage.haskell.org/package/hspec-attoparsec-0.1.0.2)