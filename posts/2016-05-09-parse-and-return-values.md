---
title: Parse and Return Values
---

Let's try to parse something a bit more interesting. In many files and programs we run across key-value pairs. In this case we will use pre-defined keys to access an unknown values. Here is a simple example:

`name:Mercutio`

The first rule for this item is it always starts with `name:`. This first parser is exactly the same pattern as the last lesson.

```haskell
parseNameKey :: Parser ()
parseNameKey = do 
  string "name:"
  return ()
```

You can try it out with a test and prove to yourself that it parses `name:`. The second rule leads us to a new type of parser. What follows `name:` is any collection of characters until the end of the input.

```haskell
parseNameValue :: Parser Text
parseNameValue = do
  n <- takeTill isEndOfLine
  return n
```

We want to collect the rest of input and return it as `Text`. `takeTill` consumes input until it parses the parser on the right or it runs out of input. Now we combine the two parsers to create a parser that can take `name:Mercutio` and return `Mercutio`.

```haskell
parseNameKeyValuePair :: Parser Text
parseNameKeyValuePair = do
  parseNameKey
  parseNameValue
```

Create some tests and try various key-value name pairs. Does it parse the way you expect it to? Now let's add a second key-value pair that has a different value type: 

`phone:867-5309`


The rules for a phone number's valuess are: consists of digits 0-9 and the dash character `-`, a dash cannot be the first or the last character, and there cannot be two dashes in sequence. These are some legal phone numbers: `489-4608`, `123456789`, `0937-876-321`. These are some illegal phone numbers: `485-32-`, `-123`, `12--232323`. Let's start with simple parser that handles only numbers.

```haskell
parsePhoneValue :: Parser Text
parsePhoneValue = do
  number <- takeWhile1 isDigit
  endOfLine <|> endOfInput
  return number
```

`takeWhile1` takes a function which takes a `Char` and returns a `Bool`:  `(Char -> Bool)`. It consumes input as long as the function it took returns `True`. It must consume at least once or the parser will fail. `isDigit` matches `0-9`. It is defined in `Data.Char`. `endOfLine <|> endOfInput` will succeed if the character is `\n`, `\r` or there is no more input. `<|>` means try the parser on the left first and if it fails, try the parser on the right. Now let's make it handle dashes appropriately.

```haskell
parsePhoneValue :: Parser Text
parsePhoneValue = do
  noDashNumbers <- takeWhile1 isDigit
  number <- many getNums
  return $ T.concat (T.append noDashNumbers number)

getNums :: Parser Text
getNums = do
  number <- takeWhile isDigit
  mDash <- (Just <$> string "-") <|> pure Nothing
  case mDash of
    Nothing -> return number
    Just dash -> do
      postDashNumber <- digit
      return $ T.append number $ T.append dash (T.pack [postDashNumber])          
```

`takeWhile1 isDigit` gets at least one digit until the first dash or end of input. `many` runs the parser until it fails and returns the parsed value. The `getNums` function parses a sequence of zero or more numbers, and optionally gets a single dash and one or more numbers at the end of the sequence. `(Just <$> string "-") <|> pure Nothing` parses a dash and wraps it in `Just` or if there is no dash then it returns `Nothing`. If it finds a dash then there must be another number after the dash or the parser will fail because we do not want a dash to be the last character. 

Try testing it on various number sequences that should pass or fail. Finally, we combine the two key-value parsers and return a new type.

```haskell
parsePersonData :: Parser Person
parsePersonData = do
  name <- parseNameKeyValue
  phone <- parsePhoneKeyValue
  return $ Person name phone 

data Person = Person {
  _getName  :: Text
, _getPhone :: Text
}
```

References:

[Data.Char defines many isTypeOfChar functions](https://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Char.html)

