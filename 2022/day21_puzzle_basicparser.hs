import Control.Applicative
import Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }


satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where 
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)


parseInt :: Parser Integer
parseInt = Parser f
    where 
      f xs
        | null ns = Nothing
        | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs


instance Functor Parser where
  fmap = inParser . fmap . fmap . first
    where inParser f = Parser . f . runParser -- runParser gets (parsed, Rest)
          first f (sep, rest) = (f sep, rest) 


instance Applicative Parser where
  pure a = Parser (\input -> Just (a, input))

  (Parser fp) <*> xp = Parser $ \s -> 
     case fp s of
        Nothing -> Nothing
        Just (f, s') -> runParser (f <$> xp) s'


instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2



{-
 - Implementation in https://www.youtube.com/watch?v=N9RUqGYuGfw for Functor
 - Recall fmap :: (a->b) -> (G a) -> (G b)
 - where G is Functor 
 -   Examples
 -     * Lists: fmap: (a->b) -> [a] -> [b], fmap f lst = map f lst
 - 
instance Functor Parser where
  fmap f (Parser p) = Parser (\input-> do
            (x, rest) <- p input
            Just (f x, rest)
-}


-- Utilities
--
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p


-- Actual parsers
ws :: Parser String
ws = zeroOrMore (satisfy isSpace)

identParser :: Parser String
identParser = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)


