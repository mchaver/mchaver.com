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
    it "should parse \"name:\"" $
      parseNameKey `shouldSucceedOn` ("name:" :: Text)
    it "should parse \"Mercutio\"" $
      parseNameValue `shouldSucceedOn` ("Mercutio" :: Text)
    it "should parse \"Mercutio\"" $
      ("Mercutio" :: Text) ~> parseNameValue `shouldParse` ("Mercutio" :: Text)
    
    it "should parse \"name:Mercutio\"" $
      ("name:Mercutio" :: Text) ~> parseNameKeyValuePair `shouldParse` ("Mercutio" :: Text)

    it "should parse \"name:Marcus Brutus\"" $
      ("name:Marcus Brutus" :: Text) ~> parseNameKeyValuePair `shouldParse` ("Marcus Brutus" :: Text)

    it "should parse \"name:Petruchio\"" $
      ("name:Petruchio" :: Text) ~> parseNameKeyValuePair `shouldParse` ("Petruchio" :: Text)
    
    it "should parse \"phone:867-5309\"" $
      ("phone:867-5309" :: Text) ~> parsePhoneKeyValuePair `shouldParse` ("867-5309" :: Text)
    

    it "should parse \"phone:867-1-5309\"" $
      ("phone:867-1-5309" :: Text) ~> parsePhoneKeyValuePair `shouldParse` ("867-1-5309" :: Text)
    

    it "should fail to parse \"phone:867--5309\"" $
      parsePhoneKeyValuePair `shouldFailOn` ("867--5309" :: Text)
    
    it "should fail to parse \"phone:-867-5309\"" $
      parsePhoneKeyValuePair `shouldFailOn` ("-867-5309" :: Text)

    it "should fail to parse \"phone:867-5309-\"" $
      parsePhoneKeyValuePair `shouldFailOn` ("867-5309-" :: Text)
      
    {-
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
  -}