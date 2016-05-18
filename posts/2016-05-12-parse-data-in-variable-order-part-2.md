---
title: Parse Data in Variable Order Part 2
---

We are going to learn how to make it easier to handle parsers that can occur in any order. The basic idea is to run multiple parsers chained with `<|>` that return the same type. We can distinguish the result of each parser with type matching. We need each parser to run exactly one time in any order so we can put the parsers in a list, keep track of which type we have already parsed, and then try all of the parsers that have not been run yet. Once all of the parsers have been run, then we can build the `Person` instance. If you try to parse, but not all of the items are there, then the parser will fail.


```haskell
data PersonField = 
  NameResult    Text |
  PhoneResult   Text |
  AddressResult Text |
  AgeResult     Int
```

This data type is used to wrap the parser results and manage the list of parsers.


```haskell
getPersonFieldParser :: PersonField -> Parser PersonField
getPersonFieldParser (NameResult    _) = NameResult  <$> parseName
getPersonFieldParser (PhoneResult   _) = PhoneResult <$> parsePhone
getPersonFieldParser (AgeResult     _) = AgeResult   <$> parseAge
```

`getPersonFieldParser` returns the appropriate parser which will wrap its type in `PersonField`. This is necessary to the types match.


```
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
```

When calling `parsePersonFields` for the first time, you need to pass an empty list `[]` to it. This means nothing has been parsed yet. `deleteItems` takes the current list of `parsedItems` and removes any item from `parsedItems` from `personFieldParsers`, which is a list of all the parsers that need to parse once in any order. `choice` attempts a list of parsers, if one succeeds then the result it is wrapped in `Just`, if it fails then it will return `Nothing`. If we get `Nothing` then we check to see if all of the items have been parsed, otherwise there has been an error. If we get `Just` then we run the parser again and pass the current list of succesful parses. 

```haskell
parsePerson :: Parser Person
parsePerson = do
  fields <- parsePersonFields []
  let mPerson = Person <$> (getNameResult fields) <*> (getPhoneResult fields) <*> (getAgeResult fields)
  
  case mPerson of
    Nothing -> fail "Failed to parse Person"
    Just person -> return person
```

This should be pretty straightforward. Run parsePersonFields, use applicative to build `Person` and each data item from `fields`. If `mPerson` is `Just` then return it unwrapped, otherwise the parser fails.

There is a lot of boilerplate code to make it all work, but it is type safe and really easy to add or remove fields to `Person` and the related parsers. Try adding a `address` to the `Person` type and necessary code for its parer.



`choice`

```
choice :: Alternative f => [f a] -> f a

choice ps tries to apply the actions in the list ps in order, until one of them succeeds. Returns the value of the succeeding action.
```