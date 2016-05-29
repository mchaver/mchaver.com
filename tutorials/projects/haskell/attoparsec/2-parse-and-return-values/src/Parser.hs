{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative 

import           Data.Attoparsec.Text
import           Data.Char

import           Data.Text (Text)
import qualified Data.Text as T

import           Prelude hiding (takeWhile)


data Person = Person {
  _getName  :: Text
, _getPhone :: Text
} deriving (Eq,Read,Show)

parsePerson :: Parser Person
parsePerson = do
  name <- parseNameKeyValuePair
  phone <- parsePhoneKeyValuePair
  return $ Person name phone

parseNameKey :: Parser ()
parseNameKey = string "name:" >> return ()

parseNameValue :: Parser Text
parseNameValue = do
  n <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return n

parseNameKeyValuePair :: Parser Text
parseNameKeyValuePair = do
  parseNameKey
  parseNameValue


parsePhoneKey :: Parser ()
parsePhoneKey = string "phone:" >> return ()


parsePhoneKeyValuePair :: Parser Text
parsePhoneKeyValuePair = do
  parsePhoneKey
  parsePhoneValue



parsePhoneValue :: Parser Text
parsePhoneValue = do
  num <- T.concat <$> many1 getNums
  return num

getNums :: Parser Text
getNums = do
  num <- takeWhile1 isDigit
  mDash <- (Just <$> string "-") <|> pure Nothing
  case mDash of
    Nothing -> return num
    Just dash -> do
      nextIsDigit
      return $ T.append num dash  


nextIsDigit :: Parser ()
nextIsDigit = do
  mNext <- peekChar
  case mNext of
    Nothing -> return () -- tried to parse the end of inpu
    Just next ->  
      if isDigit next
        then return ()
        else fail "did not find digit"
