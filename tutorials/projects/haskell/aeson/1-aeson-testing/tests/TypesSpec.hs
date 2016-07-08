{-# LANGUAGE OverloadedStrings #-}
module TypesSpec where

import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import Data.String
import Data.Text

import System.Random

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Types

userGen :: Gen User
userGen = do
  n <- arbitrary
  a <- positiveIntGen
  return $ User n (getPositive a) 

positiveIntGen :: Gen (Positive Int)
positiveIntGen = arbitrary

instance Arbitrary Text where
  arbitrary = fromString <$> (arbitrary :: Gen String)

instance Arbitrary User where
  arbitrary = userGen

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "" $ do
  it "Test arbitrary user generation and Eq for User" $ do
    x <- generate (arbitrary :: Gen User)
    x `shouldBe` x

  it "A user encoded then decoded should be equivalent to its original value" $ do
    dataJsonPairs <- forM [1..10] $ \_ -> do
      user <- generate (arbitrary :: Gen User)
      return (Just user, decode . encode $ user)

    let (users,decodedUsers) = unzip dataJsonPairs
    users `shouldBe` decodedUsers

    
    -- x <- generate (arbitrary :: Gen User)
    -- liftIO $ print (encode x)
    -- let xJsonString = encode x
    -- (Just x) `shouldBe` (decode xJsonString)

