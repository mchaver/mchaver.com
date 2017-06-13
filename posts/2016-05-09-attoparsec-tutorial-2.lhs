---
title: Attoparsec Tutorial Part 2 - Parse and Return Values
---

Now we want to parse something a little more complicated. Take a string input of
a key value pair and return the value. The input will look like this 
"name:Mercutio". The first rule is this string begins with a key `name:`.

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

parseNameKey :: Parser ()
parseNameKey = string "name:" >> return ()
\end{code}

You can try importing `Data.Attoparsec.Text`, declare `parseNameKey` and running 
`parse parseNameKey "name:"`. It will return `Done` and an empty string. To 
parse the value following the key and `:`, we only need to collect text until 
the end of line or input. `takeTill` consumes input until it parses the parser 
on the right or it runs out of input.

\begin{code}
parseNameValue :: Parser Text
parseNameValue = do
  n <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return n
\end{code}

Now we can combine the two parsers into one and get the value back.

\begin{code}
parseNameKeyValuePair :: Parser Text
parseNameKeyValuePair = do
  parseNameKey
  parseNameValue
\end{code}

Now we will parse a different key value pair, a phone number `phone:867-5309`.
Parsing the phone key is the same as parsing the name key, 

\begin{code}
parsePhoneKey :: Parser ()
parsePhoneKey = string "phone:" >> return ()
\end{code}

The rules for a phone number's values are: it consists of digits 0-9 and the 
dash character -, a dash cannot be the first or the last character, and there 
cannot be two dashes in sequence. These are some legal phone numbers: 489-4608, 
123456789, 0937-876-321. These are some illegal phone numbers: 485-32-, -123, 
12--232323. We will start with simple parser that handles only numbers.

\begin{code}
parseSimplePhoneValue :: Parser Text
parseSimplePhoneValue = do
  number <- takeWhile1 isDigit
  endOfLine <|> endOfInput
  return number
\end{code}

`takeWhile1` takes a function which takes a `Char` and returns a `Bool`: 
`(Char -> Bool)`. It consumes input as long as the function it took returns 
`True`. It must consume at least once or the parser will fail. `isDigit` matches 
0-9. It is defined in Data.Char. `endOfLine <|> endOfInput` will succeed if the 
character is `\n`, `\r` or there is no more input. `<|>` means try the parser on 
the left first and if it fails, try the parser on the right. It is part of the 
`Alternative` type class. Now we will make it handle dashes appropriately.

\begin{code}
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
    Nothing -> return () -- tried to parse the end of input
    Just next ->  
      if isDigit next
        then return ()
        else fail "did not find digit"

parsePhoneValue :: Parser Text
parsePhoneValue = do
  num <- T.concat <$> many1 getNums
  return num
\end{code}

`parsePhoneValue` runs `many1 getNums` to parse one or more series of numbers 
that can be optionally followd by a dash in the case that there is another digit 
after the dash. The `T.concat <$>` part will concat all of the number strings 
after parsing.

`getNums` collects a a series of one or more digits with `takeWhile1 isDigit`. 
`takeWhile1` takes a (Char -> Bool) function and consumes input until the 
function returns `False`. `(Just <$> string "-") <|> pure Nothing` checks if 
there is a dash and wraps it in `Just` if it succeeds, otherwise, it returns 
`Nothing`. `Nothing` has to be returned with `pure` or `return` because parsers 
are monads. `(<|>)` is an infix operator that tries a parser on the left, if 
it succeeds then it moves on, if it fails, it tracks the text input to the point 
before the left hand parser ran and then it performs the action on the right. If 
there is no dash, then we can return the series of digits. Otherwise, we use 
`nextIsDigit` to peek at the next character without consuming it. If it is a 
digit than the parser will succeed and return the number series and dash. If it 
is not, then the `getNums` parser will fail.

`nextIsDigit` peeks at the next character with `peekChar`. If it encounters the 
end of input it return `Nothing` and the `nextIsDigit` parser fails, otherwise 
if the the value of `isDigit next` is `True` then the parser succeeds, otherwise 
it will fail.

Finally, we can make a parser that combines the phone key and value parser.

\begin{code}
parsePhoneKeyValuePair :: Parser Text
parsePhoneKeyValuePair = do
  parsePhoneKey
  parsePhoneValue
\end{code}

We can also make a product type of the two values that we can parse now and 
create a parser that combines the parsers of those two values.

\begin{code}
data Person = Person
  { name  :: Text
  , phone :: Text
  } deriving (Eq, Read, Show)

parsePersonData :: Parser Person
parsePersonData = do -- Person <$> parseNameKeyValuePair <*> parsePhoneKeyValuePair
  personName  <- parseNameKeyValuePair
  personPhone <- parsePhoneKeyValuePair
  return $ Person personName personPhone
\end{code}

Continuing with the idea of test driven development, here are the specs to show 
it works against a variety of simple tests.

\begin{code}
spec :: Spec
spec = do
  describe "Parser" $ do
    it "should parse \"name:\"" $
      parseNameKey `shouldSucceedOn` ("name:" :: Text)
    it "should parse \"Mercutio\"" $
      parseNameValue `shouldSucceedOn` ("Mercutio" :: Text)
    it "should parse \"Mercutio\"" $
      ("Mercutio" :: Text) ~> parseNameValue `shouldParse` ("Mercutio" :: Text)
    
    it "should parse \"name:Mercutio\"" $
      ("name:Mercutio" :: Text) ~> parseNameKeyValuePair `shouldParse` ("Mercutio" :: Text)

    it "should parse \"name:Marcus Brutus\"" $
      ("name:Marcus Brutus" :: Text) ~> parseNameKeyValuePair `shouldParse` ("Marcus Brutus" :: Text)

    it "should parse \"name:Petruchio\"" $
      ("name:Petruchio" :: Text) ~> parseNameKeyValuePair `shouldParse` ("Petruchio" :: Text)
    
    it "should parse \"phone:867-5309\"" $
      ("phone:867-5309" :: Text) ~> parsePhoneKeyValuePair `shouldParse` ("867-5309" :: Text)
    

    it "should parse \"phone:867-1-5309\"" $
      ("phone:867-1-5309" :: Text) ~> parsePhoneKeyValuePair `shouldParse` ("867-1-5309" :: Text)
    

    it "should fail to parse \"phone:867--5309\"" $
      parsePhoneKeyValuePair `shouldFailOn` ("867--5309" :: Text)
    
    it "should fail to parse \"phone:-867-5309\"" $
      parsePhoneKeyValuePair `shouldFailOn` ("-867-5309" :: Text)

    it "should fail to parse \"phone:867-5309-\"" $
      parsePhoneKeyValuePair `shouldFailOn` ("867-5309-" :: Text)

main :: IO ()
main = hspec spec
\end{code}
