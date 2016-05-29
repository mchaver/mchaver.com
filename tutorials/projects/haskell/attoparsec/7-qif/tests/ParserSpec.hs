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
    it "parseQIFHeader" $
      parseHeader `shouldSucceedOn` ("!Type:Bank" :: Text)

    it "parseTestParser" $ 
      testParser `shouldSucceedOn` ("D6/1/94\n":: Text )

    
    it "parseNonInvestment" $
      parseNonInvestment `shouldSucceedOn` ("D6/1/94\nT75.00\nPDeposit\n^" :: Text)

    it "parseNonInvestment" $
      parseNonInvestment `shouldSucceedOn` ("D6/1/94\nT75.00\nPDeposit\nN1005\n^" :: Text)

    it "parseNonInvestment" $
      parseNonInvestment `shouldSucceedOn` ("D6/1/94\nT75.00\nPDeposit\nLstuff\n^" :: Text)

{-
    it "should parse the phrase 'Hello World!'" $
      parseHelloWorld `shouldSucceedOn` ("Hello World!" :: Text)

    it "should parse the phrase 'Hello World!' and return ()" $
      ("Hello World!" :: Text) ~> parseHelloWorld `shouldParse` ()
    
    it "should fail to parse any other string" $ do
      parseHelloWorld `shouldFailOn` ("Goodnight Everyone!" :: Text)      
-}    
main :: IO ()
main = hspec spec
