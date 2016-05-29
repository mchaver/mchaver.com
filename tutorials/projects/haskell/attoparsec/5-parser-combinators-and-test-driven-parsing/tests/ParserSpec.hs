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

    it "should parse the phrase 'Hello World!' and return ()" $
      ("Hello World!" :: Text) ~> parseHelloWorld `shouldParse` ()
    
    it "should parse the phrase 'Hello World!' and return 'Hello World!'" $
      ("Hello World!" :: Text) ~> parseHelloWorld2 `shouldParse` ("Hello World!" :: Text)
    
    it "should fail to parse any other string" $ do
      parseHelloWorld `shouldFailOn` ("Goodnight Everyone!" :: Text)      

  describe "A Parser Combinator" $ do
    it "shoul parse 'Hello World!'" $
      ("Hello World!" :: Text) ~> parseHelloWorld3 `shouldParse` ("Hello World!" :: Text)