{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Tutorial.Generic.TypeDataSpec where

import Control.Monad.IO.Class
import Data.Data
import Data.Int
import Data.Proxy
import GHC.Generics
import Tutorial.Generic.TypeData
import Test.Hspec

data Record = Record { recordId :: Int, recordName :: String }
  deriving (Show,Generic)

data Person = Person { name :: String, age :: String }
  deriving (Show,Generic)

data SumType = SumTypeRecord { getRecord :: Record}
             | SumTypePerson { getPerson :: Person}
  deriving (Show,Generic)

spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      let d = selectors2 (Proxy :: Proxy (Rep Record))
      liftIO $ print d
      let s = selectors2 (Proxy :: Proxy (Rep SumType))
      liftIO $ print s
      True `shouldBe` True

main :: IO ()
main = hspec spec


{-
:kind! Rep Record
Rep Record :: * -> *
= D1
    Main.D1Record
    (C1
       Main.C1_0Record
       (S1 Main.S1_0_0Record (Rec0 Int32)
        :*: S1 Main.S1_0_1Record (Rec0 ByteString)))
-}
