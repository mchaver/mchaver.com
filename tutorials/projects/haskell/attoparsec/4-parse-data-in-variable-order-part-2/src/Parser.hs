{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative 

import           Data.Attoparsec.Text
import           Data.Char
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T

import           Prelude hiding (takeWhile)

import           Safe

-- readMay :: (Maybe Int)
data Person = Person {
  _getName  :: Text
, _getPhone :: Text
, _getAge   :: Int
} deriving (Eq,Read,Show)

parsePerson :: Parser Person
parsePerson = do
  fields <- parsePersonFields []
  let mPerson = Person <$> (getNameResult fields) <*> (getPhoneResult fields) <*> (getAgeResult fields)
  
  case mPerson of
    Nothing -> fail "Failed to parse Person"
    Just person -> return person

data PersonField = 
  NameResult    Text |
  PhoneResult   Text | 
  AgeResult     Int 
  deriving (Eq,Read,Show)

parseName :: Parser Text
parseName = do
  _ <- string "name:"
  n <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return n


parsePhone :: Parser Text
parsePhone = do
  _ <- string "phone:"
  num <- T.concat <$> many1 getNums
  endOfLine <|> endOfInput
  return num

parseAge :: Parser Int
parseAge = do
  _ <- string "age:"
  age <- T.unpack <$> takeWhile1 isDigit
  let mAgeInt = readMay age :: (Maybe Int)
  endOfLine <|> endOfInput
  case mAgeInt of
    Nothing -> fail "Did not parse an Int"
    Just ageInt -> return ageInt

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



getNameResult :: [PersonField] -> Maybe Text
getNameResult (x:xs) = 
  case x of 
    (NameResult y) -> Just y
    _ -> getNameResult xs
getNameResult _ = Nothing

getPhoneResult :: [PersonField] -> Maybe Text
getPhoneResult (x:xs) = 
  case x of 
    (PhoneResult y) -> Just y
    _ -> getPhoneResult xs
getPhoneResult _ = Nothing

getAgeResult :: [PersonField] -> Maybe Int
getAgeResult (x:xs) =
  case x of
    (AgeResult y) -> Just y
    _ -> getAgeResult xs
getAgeResult _ = Nothing

getPersonFieldParser :: PersonField -> Parser PersonField
getPersonFieldParser (NameResult    _) = NameResult  <$> parseName
getPersonFieldParser (PhoneResult   _) = PhoneResult <$> parsePhone
getPersonFieldParser (AgeResult     _) = AgeResult   <$> parseAge
-- getPersonFieldParser (AddressResult _) = parseAddress


deleteItems :: (Eq a) => [a] -> [a] -> [a]
deleteItems (x:xs) ys = deleteItems xs $ delete x ys 
deleteItems _ ys = nub ys

personFieldParsers :: [PersonField]
personFieldParsers = [NameResult "", PhoneResult "", AgeResult 0]

parsePersonFields :: [PersonField] -> Parser [PersonField]
parsePersonFields parsedItems = do
  let parsers = deleteItems parsedItems personFieldParsers
  mResult <- (Just <$> choice (map getPersonFieldParser parsers)) <|> return Nothing
  
  case mResult of
    Nothing -> 
      if (length parsedItems) == (length personFieldParsers) 
      then return parsedItems
      else fail "Not all items were parsed"
    Just result -> parsePersonFields (parsedItems ++ [result])
