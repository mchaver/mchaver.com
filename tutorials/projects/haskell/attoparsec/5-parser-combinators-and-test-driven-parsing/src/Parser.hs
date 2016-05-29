{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Attoparsec.Text
import           Data.Text (Text)
import qualified Data.Text as T

parseHelloWorld :: Parser ()
parseHelloWorld = do
  _ <- string "Hello World!"
  return ()

parseHelloWorld2 :: Parser Text
parseHelloWorld2 = string "Hello World!"

parseHello :: Parser Text
parseHello = string "Hello"

parseWorld :: Parser Text
parseWorld = string " World!"

parseHelloWorld3 :: Parser Text
parseHelloWorld3 = do
  hello <- parseHello
  world <- parseWorld
  return $ T.concat [hello,world]
