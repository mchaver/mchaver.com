---
title: Attoparsec Tutorial Part 1 - Parser Combinators and Test Driven Parsing
tags: haskell, attoparsec
---

[Attoparsec](http://hackage.haskell.org/package/attoparsec) is a 
Haskell parser combinator library for parsing `ByteString` and `Text`. We can
use it to make small parsers and combine these small parsers to create more 
complex ones. The advantage of this style is that we can individually test 
each parser, making it easier to debug and build, as well as make the 
individual pieces reusable.

We are going to focus on parsing `Text` from `Data.Text`.

\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.Text
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.Attoparsec

parseHelloWorld :: Parser ()
parseHelloWorld = do
  _ <- string "Hello World!"
  return ()
\end{code}

Though you cannot tell by looking at it, `Parser ()` takes a `Text` and returns `()`. 
`Parser` is a type synonym for a more complex type that takes a type `i`, 
manages some state, and returns a parsed value of your choice. Running a parser 
will return `Either Text a`, where `Text` is an error message and 
`a` is whatever we decide to return. In this case it is `()`. 

`string` takes a `Text` and returns it in a `Parser`, but we are ignoring the 
value returned from `string` because our parser returns `()`. In a later lesson 
we will explore the Parser type declaration. It is worth taking a look at the 
definition in the 
[source code](https://hackage.haskell.org/package/attoparsec-0.13.1.0/docs/src/Data-Attoparsec-Internal-Types.html#Parser).

Now we will make a test to prove to ourselves that the parser works. 

\begin{code}
spec1 :: Spec
spec1 = do
  describe "parseHelloWorld" $ do
    it "should parse the phrase 'Hello World!'" $
      parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)
\end{code}

`shouldSucceedOn` runs a parser from the left hand side with the 
input on the right hand side and allows the test to pass if the parser returns 
`Right`. Do you want to see what a failed test looks like? Change the right hand
side of `shouldSucceedOn` to `"Hello World"` and you will see a 
`"not enough input"` error. The `string` function could not find 
the last character `!`. Let's slightly change the `parseHelloWorld` function 
so we can show off some more functionallity.

\begin{code}
parseHelloWorld2 :: Parser Text
parseHelloWorld2 = do
  result <- string "Hello World!"
  return result
\end{code}

Now the parse will return the result from the `string` function. We can verify 
this with another test.

\begin{code}
spec2 :: Spec
spec2 = do
  describe "parseHelloWorld2" $ do 
    it "should parse the phrase 'Hello World!' and return 'Hello World!'" $
      ("Hello World!" :: Text) ~> parseHelloWorld2 `shouldParse` ("Hello World!" :: Text)
\end{code}

The `(~>)` function feeds a `Text` value to be parsed by `parseHelloWorld2` from 
the left hand side and `shouldParse` says not only should the parser succeed, 
but its result should match the right hand side. There are two ways this can 
fail. `parseHelloWorld2` could fail to parse the left hand side, or it could 
succeed in parsing it but it does not return the expectation on the right hand 
side. Try deleting something from the left or right hand side and see what 
happens when you run the test.

Sometimes we want to be able to prove that a parser will fail, just use `shouldFailOn`.

\begin{code}
    it "should fail to parse any other string" $ do
      parseHelloWorld `shouldFailOn` ("Goodnight Everyone!" :: Text)
\end{code}

For failed cases we do not need to inspect the returned result because we just 
get an error message, the left hand side from `Either Text a` that the 
parser returns. Finally, here is what a parser combinator looks like. We will 
split up the original function and glue it together in another one.

\begin{code}
parseHello :: Parser Text
parseHello = string "Hello"

parseWorld :: Parser Text
parseWorld = string " World!"

parseHelloWorld3 :: Parser Text
parseHelloWorld3 = do
  hello <- parseHello
  world <- parseWorld
  return $ T.concat [hello,world]
\end{code}

Add a new test for it.

\begin{code}
spec3 :: Spec
spec3 = do
  describe "parseHelloWorld3" $ do
    it "should parse 'Hello World!'" $
      ("Hello World!" :: Text) ~> parseHelloWorld3 `shouldParse` ("Hello World!" :: Text)

main :: IO ()
main = do 
  hspec spec1
  hspec spec2
  hspec spec3
\end{code}

You can run this file with the following command: `stack --resolver lts-8.17 runghc 2016-05-09-attoparsec-tutorial-1.lhs`.

=== References:

- [Hackage :: attoparsec](http://hackage.haskell.org/package/attoparsec-0.13.10.0/docs/Data-Attoparsec-Text.html)

- [Hackage :: hspec](http://hackage.haskell.org/package/hspec)

- [Hackage :: hspec-attoparsec](http://hackage.haskell.org/package/hspec-attoparsec)
