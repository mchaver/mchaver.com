---
title: Parse Data in Variable Order Part 1
date: 2016-05-10
---

[Source Code](https://github.com/mchaver/mchaver.com/tree/master/tutorials/projects/haskell/attoparsec/3-parse-data-in-variable-order-part-1)

In this lesson will be building upon the parser from the previous lesson. Our goal is to take a string in which `name` and `phone` can occur in any order and output a `Person`.

```haskell
data Person = Person {
  _getName  :: Text
, _getPhone :: Text
} deriving (Eq,Read,Show)
```

String with `name` first:
```
name:Isaac
phone:1122-3344
```

String with `phone` first:
```
phone:1122-3344
name:Isaac
```

My first intuition would be to start with a parser similar to this:

```haskell
parsePerson :: Parser Person
parsePerson = do
  first <- parseName <|> parsePhone
```

The problem is how do we distinguish between a name and a phone? They both return `Text` so in terms of types they look the same. A simple solution is to create a sum type that can represnt a name or a phone, then we can type match the parse result.

```haskell
data PersonItem = NameResult Text | PhoneResult Text

parsePerson :: Parser Person
parsePerson = do
  first <- (NameResult <$> parseName) <|> (PhoneResult <$> parsePhone)
```

We try to perform `parseName` and wrap it in `NameResult`. If `parseName` fails then we try `parsePhone` and wrap it in `PhoneResult`. If that fails also then `parsePerson` fails. Keep in mind that both parsers used in `(<|>)` must return the same type. We have met this requirement by wrapping the parse result of the two parses with constructors of of the same parse type. Now we can add some behavior based on which type is parsed first.

```haskell
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
```

If name is parsed first then we parse the phone number, and if phone number is parsed then we parse the name. The end result is the same regardless of which key-value pair occurs first. However, this pattern does not scale very well. Think about what it would like if we had four or five constructors in `PersonItem`. The code would become very messy.
