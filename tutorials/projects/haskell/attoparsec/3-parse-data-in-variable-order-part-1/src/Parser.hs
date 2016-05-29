{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative 

import           Data.Attoparsec.Text
import           Data.Char

import           Data.Text (Text)
import qualified Data.Text as T

import           Prelude hiding (takeWhile)

parseNameKey :: Parser ()
parseNameKey = string "name:" >> return ()

parseNameValue :: Parser Text
parseNameValue = do
  n <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return n

parseName :: Parser Text
parseName = do
  parseNameKey
  parseNameValue


parsePhoneKey :: Parser ()
parsePhoneKey = string "phone:" >> return ()


parsePhoneValue :: Parser Text
parsePhoneValue = do
  number <- many1 getNums
  endOfLine <|> endOfInput
  return $ T.concat number


getNums :: Parser Text
getNums = do
  number <- takeWhile1 isDigit
  mDash <- (Just <$> string "-") <|> pure Nothing
  case mDash of
    Nothing -> return number
    Just dash -> do
      postDashNumber <- digit
      return $ T.append number $ T.append dash (T.pack [postDashNumber])

parsePhone :: Parser Text
parsePhone = do
  parsePhoneKey
  parsePhoneValue


data Person = Person {
  _getName  :: Text
, _getPhone :: Text
} deriving (Eq,Read,Show)

data PersonItem = NameResult Text | PhoneResult Text

parsePerson :: Parser Person
parsePerson = do
  first <- (NameResult <$> parseName) <|> (PhoneResult <$> parsePhone)
  case first of
    NameResult name -> do
      phone <- parsePhone
      return $ Person name phone
    PhoneResult phone -> do
      name <- parseName
      return $ Person name phone
