{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (main, spec) where

-- import Data.Attoparsec
import Data.Text (Text)
import Parser

import Test.Hspec
import Test.Hspec.Attoparsec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "should parse the phrase 'Hello World!'" $
      parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)

    it "should parse the phrase 'Hello World!' and return ()" $
      ("Hello World!" :: Text) ~> parseHelloWorld `shouldParse` ()
    
    it "should fail to parse any other string" $ do
      parseHelloWorld `shouldFailOn` ("Goodnight Everyone!" :: Text)      
  
  describe "parseRose" $ do
    it "should parse a single digit" $
      ("5" :: Text) ~> parseRose `shouldParse` (Rose 5 [])
    it "should parse a single digit" $
      ("5\n  6" :: Text) ~> parseRose `shouldParse` (Rose 5 [Rose 6 []])
    it "should parse a single digit" $
      ("5\n  6\n  7" :: Text) ~> parseRose `shouldParse` (Rose 5 [Rose 6 [], Rose 7 []])
    it "should parse a single digit" $
      ("5\n  6\n    7\n  8" :: Text) ~> parseRose `shouldParse` (Rose 5 [Rose 6 [Rose 7 []], Rose 8 []])
    it "should parse a single digit" $
      ("1\n  2\n    3\n      4\n    5\n    6\n  7\n  8\n" :: Text) ~> parseRose `shouldParse` (Rose 1 [Rose 2 [Rose 3 [Rose 4 []], Rose 5 [], Rose 6 []], Rose 7 [], Rose 8 []])


{-
1
  2
    3
      4
    5
    6
  7
  8
-}
main :: IO ()
main = hspec spec
