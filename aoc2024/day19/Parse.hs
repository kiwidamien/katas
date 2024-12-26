module Parse (Token, parseTokens, parse) where 

type Token = String

parseTokens :: String -> [Token]
parseTokens line = words $ map (\c -> if c == ',' then ' ' else c) line

parse :: String -> ([Token], [String])
parse s = let orderedLines = lines s in (parseTokens $ head orderedLines, drop 2 orderedLines)