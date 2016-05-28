---
title: Parsing Required and Optional Data in Variable Order
---

A special case of variable order is having required and optional items that can occur. The strategy we will use is based upon the previous lessons. We need two lists: one for the required parsers and one for the optional parsers. We maintain a list of parsed items, we get parses that have not been parsed yet, then if a parse succeeds, we append our list of parsed items and try to parse once more, if it fails, then we make sure all of the items we are parsed are also in the list of required parses, we can ignore the optional parses. 

Also, if we can a parser that can be parsed in zero or more times in any order. We keep it in a third list of parses that we always try for, we get the list of unused parses, then append our parses that can parse zero or more times.