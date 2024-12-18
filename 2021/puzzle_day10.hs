import Data.List

test=["[({(<(())[]>[[{[]{<()<>>",
  "[(()[<>])]({[<{<<[]>>(",
  "{([(<{}[<>[]}>{[]{[(<()>",
  "(((({<>}<{<{<>}{[]{[]{}",
  "[[<[([]))<([[{}[[()]]]",
  "[{[{({}]{}}([{[{{{}}([]",
  "{<[[]]>}<{[{[{[]{()[[[]",
  "[<(<(<(<{}))><([]([]()",
  "<{([([[(<>()){}]>(<<{{",
  "<{([{{}}[<[[[<>{}]]]>[]]"]

eliminate :: Char -> Char -> String -> String
eliminate _ _ "" = ""
eliminate _ _ (c:[]) = [c]
eliminate open close (pot_open:pot_close:rest)
    | pot_open /= open = pot_open:(eliminate open close (pot_close:rest))
    | pot_close == close = eliminate open close rest
    | otherwise = pot_open:(eliminate open close (pot_close:rest))

eliminateCycle :: String -> String
eliminateCycle s = (eliminate '(' ')' $ eliminate '[' ']' $ eliminate '<' '>' $ eliminate '{' '}' s)


reduceString :: String -> String
reduceString s = if (length s == length reduced) then reduced else reduceString reduced
  where reduced = eliminateCycle s

findFirstIllegalChar :: String -> Char 
findFirstIllegalChar s 
  | (length closed) > 0 = head closed
  | otherwise = ' ' 
  where closed = filter (\x->elem x ")}]>") $ reduceString s

scoreError :: String -> Int
scoreError s
  | illegal==')' = 3
  | illegal==']' = 57
  | illegal=='}' = 1197
  | illegal=='>' = 25137
  | otherwise = 0
  where illegal = findFirstIllegalChar s

autocomplete :: String -> String
autocomplete beg 
  | illegal = "" 
  | otherwise = reverse $ reduced
  where reduced = reduceString beg
        illegal = foldl (||) False $ map (\x->elem x ")}>]") reduced  

scoreAutocomplete :: String -> Int
scoreAutocomplete s = go 0 ac
  where ac = autocomplete s
        go curr str
            | str=="" = curr 
            | otherwise = go (5*curr + (cscore (head str))) (tail str)
        cscore c
            | c == '(' = 1
            | c == '[' = 2
            | c == '{' = 3
            | c == '<' = 4

scoreAutocompleteCollection :: [String] -> Int
scoreAutocompleteCollection strs = median sortedScores
  where sortedScores = sort $ filter (>0) $ map scoreAutocomplete strs
        median lst = lst !! (div (length lst) 2) -- abusing all lists are odd length


part1 = do
    contents <- lines <$> readFile "inputs/day10.txt"
    let scores = sum $ map scoreError contents 
    return scores


part2 = do
    contents <- lines <$> readFile "inputs/day10.txt"
    let scores = scoreAutocompleteCollection contents
    return scores
