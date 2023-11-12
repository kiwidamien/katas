module Day19 where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M

data Rule = Terminal String | Reference Int | RuleChoice [Rule] | RuleSequence [Rule] deriving (Show)

type RuleSet = Map Int Rule

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = let (p1, p2) = break (== c) s in p1 : (splitOn c $ drop 1 p2)

process :: String -> RuleSet -> Rule -> Maybe String
process s _ (Terminal r) =
  let charToMatch = length r
      (beg, end) = (take charToMatch s, drop charToMatch s)
   in if (beg == r) then (Just end) else Nothing
process s ruleset (Reference index) =
  let newRule = M.findWithDefault (Terminal "") index ruleset
   in process s ruleset newRule
process s ruleset (RuleChoice rules) = msum $ map (\r -> process s ruleset r) rules
process s ruleset (RuleSequence []) = Just s
process s ruleset (RuleSequence (r : ules)) =
  let s' = process s ruleset r
   in case (s') of
        Nothing -> Nothing
        (Just new_s) -> process new_s ruleset (RuleSequence ules)

satisfied :: String -> RuleSet -> Int -> Bool
satisfied s ruleset index = (process s ruleset rule) == (Just "")
  where
    rule = M.findWithDefault (Terminal "") index ruleset

parseLine :: String -> (Int, Rule)
parseLine s = (read prefix, go ruleSpec)
  where
    (prefix, _ : _ : ruleSpec) = break (== ':') s
    go :: String -> Rule
    go spec
      | elem '|' spec = RuleChoice (map go $ splitOn '|' spec)
      | elem ' ' spec = RuleSequence (map go $ words spec)
      | elem '"' spec = Terminal (filter (/= '"') spec)
      | otherwise = Reference (read spec)

-- satisfy :: String -> RuleSet -> Rule -> Bool
parseRuleset :: [String] -> RuleSet
parseRuleset = M.fromList . map parseLine

exampleRules :: RuleSet
exampleRules = parseRuleset ["0: 1 2", "1: \"a\"", "2: 1 3 | 3 1", "3: \"b\""]

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let (rules, _ : messages) = break (== "") input
  let ruleset = parseRuleset rules
  let numSatisfy = length $ filter (\r -> satisfied r ruleset 0) messages
  print (numSatisfy)
