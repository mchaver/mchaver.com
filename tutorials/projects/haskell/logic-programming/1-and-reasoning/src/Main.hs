{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map.Strict as Map

data Query = Query {
  queryRule   :: Text
, queryValues :: [Text]
} deriving (Eq,Show)

type Rules = Map.Map Text Rule

data Rule = Rule {
  ruleName  :: Text
, ruleVars  :: [Text]
, ruleFacts :: Facts
} deriving (Eq,Show)

type Facts = Map.Map Text Fact

data Fact = Fact {
  factName   :: Text
, factValues :: [Text]
} deriving (Eq,Show)

runQuery :: Query -> Rules -> Facts -> Bool
runQuery q rs fs = True
  where
    mRule = Map.lookup (queryRule q) rs
    vars  = ruleVars <$> mRule
    -- sameLength = (== (length queryValues)) <$> ()
    -- mRuleFacts = ruleFacts <$> mRule
matchQueryToRules :: Query -> Rule -> Facts -> Bool
matchQueryToRules q r fs = True
  where
    vars = ruleVars r
    sameLength = length (queryValues q) == length vars
    varNames = ruleVars r
    varValues = queryValues q
    -- varName as key, varValue as lookup return
    varNameValues = Map.fromList $ (,) <$> varNames <*> varValues
    -- match var name from rule, to var name in body

    -- for each rule in the rule body
    --   lookup the facts by name
    --   get the bound varName - varValues
    --   see if fact exists

-- already matched factName, get var names
lookupFact :: Map.Map Text Text -> Fact -> Bool
lookupFact varNameValues f = True
  where
    mVals = Map.lookup "" varNameValues -- $ factValues f-- (Map.lookup varNameValues) <$> (factValues f)
    mm    = map (flip Map.lookup varNameValues) (factValues f)-- Map.lookup () f

main :: IO ()
main = do
  print "Hello"


{-
only facts in body

likes(Harold,Kumar)
likes(Kumar,Harold)
likes(Todd)
likes(Todd,Harold)
-- likes(Harold,Todd)

likes(Adrian,Kumar)
-- likes(Kumar,Adrian)

friends(?X,?Y) :-
  likes(?X,?Y)
  likes(?Y,?X)

friends(?X,Thomas) :-
  likes(?X,Thomas)  ; check facts
  likes(Thomas,?X)  ; check facts

-}
