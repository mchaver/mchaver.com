{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Attoparsec.Text
import Data.Text (Text)

parseHelloWorld :: Parser ()
parseHelloWorld = do
  _ <- string "Hello World!"
  return ()
