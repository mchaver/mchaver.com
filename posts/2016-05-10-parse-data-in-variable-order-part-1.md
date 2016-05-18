---
title: Parse Data in Variable Order Part 1
---

In this lesson will be building on the parser from the previous lesson. Our goal is to take text in which `person` and `phone` can occur in any order and output a `Person`.

```haskell
data Person = Person {
  _getName  :: Text
, _getPhone :: Text
} deriving (Eq,Read,Show)
```

```
name:
phone:
```

```
phone:
name:
```

First intuition might be something like this:

```haskell
parsePerson :: Parser Person
parsePerson = do
  first <- parseName <|> parsePhone
```

The problem is how do we distinguish between a name and a phone? They both return Text so in terms of types they look the same. We need to give them unique constructors of the same type so we can match on the type and choose an action based on the previous type. 

```haskell
data PersonItem = NameResult Text | PhoneResult Text

parsePerson :: Parser Person
parsePerson = do
  first <- (NameResult <$> parseName) <|> (PhoneResult <$> parsePhone)
```

This should be pretty straight forward. We `parseName` and wrap it in `NameResult`, and `parsePhone` and wrap it in `PhoneResult`. Keep in mind that both parsers used in `(<|>)` must return the same type. Now we can add some behavior based on which type is parsed first. 

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