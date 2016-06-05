---
title: Parse and Return Values
---

[Source Code](https://github.com/mchaver/mchaver.com/tree/master/tutorials/projects/haskell/attoparsec/2-parse-and-return-values)

We will parse something a bit more interesting in this lesson. In many files and programs we run across key-value pairs. For this example, we will use pre-defined keys to access an unknown values. Here is a simple example:

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


The rules for a phone number's values are: it consists of digits 0-9 and the dash character `-`, a dash cannot be the first or the last character, and there cannot be two dashes in sequence. These are some legal phone numbers: `489-4608`, `123456789`, `0937-876-321`. These are some illegal phone numbers: `485-32-`, `-123`, `12--232323`. Let's start with simple parser that handles only numbers.

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
```

`parsePhoneValue` runs `many1 getNums` to parse one or more series of numbers that can be optionally followd by a dash in the case that there is another digit after the dash. The `T.concat <$>` part will concat all of the number strings after parsing.

`getNums` collects a a series of one or more digits with `takeWhile1 isDigit`. `takeWhile1` takes a (Char -> Bool) function and consumes input until the function returns `False`. `(Just <$> string "-") <|> pure Nothing` checks if there is a dash and wraps it in `Just` if it succeeds, otherwise, it returns `Nothing`. `Nothing` has to be returned with `pure` or `return` because parsers are monads. `(<|>)` is an infix operator that tries a parser on the left, if it it succeeds then it moves on, if it fails, it tracks the text input to the point before the left hand parser ran and then it performs the action on the right. If there is no dash, then we can return the series of digits. Otherwise, we use `nextIsDigit` to peek at the next character without consuming it. If it is a digit than the parser will succeed and return the number series and dash. If it is not, then the `getNums` parser will fail.

`nextIsDigit` peeks at the next character with `peekChar`. If it encounters the end of input it return `Nothing` and the `nextIsDigit` parser fails, otherwise if the the value of `isDigit next` is `True` then the parser succeeds, otherwise it will fail.

Try testing `parsePhoneValue` on various phone number sequences that should pass or fail. Finally, we combine the two key-value parsers and return a new type.

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

New Attoparsec Functions:

 * `(<|>)` infix operator that will run the parser on the left first, if it fails it does not consume any input and tries to run the parser on the right. It may be chained like this `parser1 <|> parser2 <|> parser3 <|> parser4`. The parser will be tried in order from left to right.

 * `endOfLine` succeeds if the char is `\n` or `\r`.

 * `endOfInput` succeeds when at the end of input.

 * `isDigit` returns true for `0-9`.

 * `many1` takes a parser, consume input until the parser fails, must succeed at least once, return consumed input.

 * `peekChar` returns the next char wrapped in `Just`, but does not consume it. If it is then end of input then it returns `Nothing`. This parser will not fail.

 * `peekChar'` returns the next char, but does not consume it. It will fail on the end of input

 * `takeTill` takes a (Char -> Bool) function, consume input until the function returns `True`, return consumed input.

 * `takeWhile` take a (Char -> Bool) function

 * `takeWhile1` takes a (Char -> Bool) function, consume input as long as the function returns `True`, must succeed at least once, returns consumed input.


Attoparsec parser always backtrack on failure, the rewind to their original starting point.

References:

[hackage :: Data.Char](https://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Char.html) defines many useful `(Char -> Bool)` functions

