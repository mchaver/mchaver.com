---
title: Attoparsec Tutorial Part 3 - Parse Data in Variable Order
---

In ths lesson, we will build upon the parses from the previous lesson. The goal 
is to take a string in which the `name` and `phone` key-value pairs may occur 
in any order and can succesfully be parsed into a value of type `Person`.

String with name first:

```
name:Isaac
phone:1122-3344
```

String with phone first:

```
phone:1122-3344
name:Isaac
```

My first intuition was to parse it like this:

```haskell
parsePerson :: Parser Person
parsePerson = do
  first  <- parseName <|> parsePhone
  ...
```

The problem this is unable to distinguish between the resulting type. They both 
return `Text`. The value of the key-value pairs looks the same. A simple 
solution is to create a sum type that can represent a name or a phone, then we 
can type match the parse result.

\ignore{
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative 
import           Data.Attoparsec.Text
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (takeWhile)
import           Test.Hspec
import           Test.Hspec.Attoparsec

data Person = Person
  { name  :: Text
  , phone :: Text
  } deriving (Eq,Read,Show)

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
\end{code}
} 

\begin{code}
data PersonItem = NameResult Text | PhoneResult Text

parsePerson :: Parser Person
parsePerson = do
  first <- (NameResult <$> parseName) <|> (PhoneResult <$> parsePhone)
  case first of
    NameResult n -> do
      p <- parsePhone
      return $ Person n p
    PhoneResult p -> do
      n <- parseName
      return $ Person n p
\end{code}

We try to perform `parseName` and wrap it in `NameResult`. If `parseName` fails then 
we try `parsePhone` and wrap it in `PhoneResult`. If that fails also then 
`parsePerson` fails. Keep in mind that both parsers used in `(<|>)` must return the 
same type. We have met this requirement by wrapping the parse result of the two 
parses with constructors of of the same parse type.

If name is parsed first then we parse the phone number, and if phone number is 
parsed then we parse the name. The end result is the same regardless of which 
key-value pair occurs first. However, this pattern does not scale very well. 
Think about what it would like if we had four or five constructors in 
`PersonItem`. The code would become very messy.

\begin{code}
spec :: Spec
spec = do
  describe "parsePerson" $ do
    it "should parse \"name:\"" $
      parseNameKey `shouldSucceedOn` ("name:" :: Text)
    it "should parse \"name:James\nphone:867-5309\"" $
      ("name:James\nphone:867-5309" :: Text) ~> parsePerson `shouldParse` (Person "James" "867-5309")
    it "should parse \"phone:867-5309\nname:James\"" $
      ("phone:867-5309\nname:James" :: Text) ~> parsePerson `shouldParse` (Person "James" "867-5309" :: Person)

main :: IO ()
main = hspec spec
\end{code}

Run code with `stack --resolver lts-8.17 runghc 2016-05-10-attoparsec-tutorial-3.lhs`.
