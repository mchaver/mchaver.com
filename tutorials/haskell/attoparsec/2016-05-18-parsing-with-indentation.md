---
title: Parsing With Indentation
---

parseHeader :: Parser Header
parseHeader = Header <$> takeTill isEndOfLine

parseField :: Int -> Parser (Int, Field)
parseField previousIdentSize = do
  indentSpace <- takeWhile isSpace
  let identSize = length indentSpace
  -- if previousIdentSize == -1, this is the first field
  -- identSize must be greated than 0, defines indent
  -- if identSize == previousIdentSize
  if indentSize == previousIdentSize -- same level, not child
  if indentSize >  previousIdentSize -- 
  then 
  else
  
-- zero or more
parseFields = do
  (first,indentSize) <- parseField -1
  rest <- snd <$> many parseField indentSize
  return $ [first] ++ rest
  
parseModel :: Parser Model
parseModel = do
  header <- parseHeader
  fields <- parseFields
  return $ Model header field
  

-- handle branching
-- 

-- command, command, for-loop, command


```
data RoseTree a = RoseTree a [RoseTree a]
retree = Rose 5 [Rose 3 [Rose 1 [], Rose 4[]], Rose 7 []]
5
  3
    1
    4
  7
```
parse root 5
must indent
parse 2 spaces
find 3
  parse 2 spaces = sibling
  parse > 2 = child
  spaces is greater than 2, sibling
  

parse self, parse children, children as next

programs are written in tree structures
tree like structure are very useful and found every in data formatting ASTs SEXP, Lisp, C, XML, JSON, YAML,etc. just to name a few

simple recursive format