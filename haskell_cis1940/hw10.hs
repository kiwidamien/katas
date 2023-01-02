{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1
-- Implement a Functor for Parser
--
applyToFirst :: (a -> b) -> (a,  c) -> (b, c)
applyToFirst f (x, y) = (f x, y)

-- This was copied after I got stuck
-- Idea: we have two different examples of Parser f above 
instance Functor Parser where
  fmap f (Parser rp) = Parser (fmap (applyToFirst f) . rp)


-- Exercise 2
-- Implement an Applicative instance for Parser
-- Notes:
--   * `pure` represents a Parser that consumes no input and successfully returns a result of `a`
--   * p1 <*> p2 represents a Parser that runs p1, then the remaining output on p2.
--     It succeeds if and only if p1 and p2 both succeed.
instance Applicative Parser where
  pure a = Parser f
    where f str = Just (a, str)
 
  p1 <*> p2 = Parser f
    where f str = case runParser p1 str of 
                     Nothing -> Nothing
                     Just (fRes, strRes) -> (applyToFirst fRes) <$> runParser p2 strRes

-- Exercise 3: Using Applicative
-- Make a parser that accepts 'ab' as a pair
-- AParser> runParser abParser "abcdef"
-- Just (('a', 'b'), "cdef")
-- AParser> runParser abParser "acbdef"
-- Nothing
-- Do it twice: once using the basic definition, then once using the Applicative interface
abParserBasic :: Parser (Char, Char)
abParserBasic = Parser f
  where 
      f ('a':'b':xs) = Just( (('a', 'b'), xs) )
      f _ = Nothing

-- Now let's do the applicative thing
-- abParser = (char 'a')  <*> (char 'b')
-- My initial attempts left off (,) <$>
-- This was needed as a way to collect the parsed results (Tuple operator)
abParser = (,) <$> char 'a' <*> char 'b'

-- As a different example, this takes strings of the form
-- AParser> runParser abParserJoined "abcde"
-- Just ("ab", "cde")
-- AParser> runParser abParserJoined "bacde"
-- Nothing
abParserJoined = (\f s -> [f,s]) <$> char 'a' <*> char 'b'

-- implement
-- AParser> runParser abParser_ "abcde"
-- Just ((), "cde")
-- AParser> runParser abParser_ "bacde"
-- Nothing
abParser_ = (\f s -> ()) <$> char 'a' <*> char 'b'

-- implement
-- AParser> runParser intPair "12 34"
-- Just([12, 34], "")
-- AParser> runParser intPair "12 34 abcd"
-- Just([12, 34], " abcd")
intPair = (\f _ s -> [f, s]) <$> posInt <*> char ' ' <*> posInt

-- Exercise 4: Alternatives
-- Applicatives are not enough if we want to make choices rather than fixed formats
-- e.g. after the colon there can be a word OR parenthesis
-- Use Alternative class instead
instance Alternative Parser where
  empty = Parser f
              where f _ = Nothing
  p1 <|> p2 = Parser f
              where f str = runParser p1 str <|> runParser p2 str 


-- Not sure why the naive const () <$> posInt <|> satisfy isUpper 
-- did not work
-- Ah, I get it now. One creates a Parser Int, the other a Parser Char
--   These cannot be "combined" via <|>, so
--   intToVoid :: Parser ()
--   intToVoid = const () <$> posInt
--
--   strToVoid :: Parser ()
--   strToVoid = const () <$> satisfy isUpper
--
--   can be composed via `intToVoid <|> strToVoid` 
intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper
