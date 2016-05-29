{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative 

import Data.Attoparsec.Text
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Prelude hiding (takeWhile)

parseHelloWorld :: Parser ()
parseHelloWorld = do
  _ <- string "Hello World!"
  return ()


data Rose a = Rose a [Rose a]
  deriving (Eq,Show)

parseRose :: Parser (Rose Int)
parseRose = do
  root <- many1 digit
  endOfLine <|> endOfInput
  children <- many' $ parseRose' 2
  return $ Rose (read root :: Int) children

parseRose' :: Int -> Parser (Rose Int)
parseRose' previousIndent = do
  spaces <- takeWhile isSpace
  case (T.length spaces) == previousIndent of
    True -> do
      node <- many1 digit
      endOfLine <|> endOfInput
      children <- many' $ parseRose' (previousIndent + 2)
      return $ Rose (read node :: Int) children
    False -> fail "Did not parse child"


{-
5
  6
    7
  8
-}

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