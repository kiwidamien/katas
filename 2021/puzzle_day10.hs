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

oneExample = "[({(<(())[]>[[{[]{<()<>>"


eliminate :: Char -> Char -> String -> String
eliminate open close (s1:s2:rest) = if (s1==open)&&(s2==close) then (eliminate open close rest) else (s1:(eliminate open close (s2:rest)))
eliminate open close (s:rest) = (s:eliminate open close rest)
eliminate open close "" = ""

eliminateCycle :: String -> String
eliminateCycle s = (eliminate '(' ')' $ eliminate '[' ']' $ eliminate '<' '>' $ eliminate '{' '}' s)


eliminateUntilStop :: String -> String
eliminateUntilStop s = if (length s == length reduced) then reduced else eliminateUntilStop reduced
  where reduced = eliminateCycle s

findFirstIllegalChar :: String -> Char 
findFirstIllegalChar s = snd $ safeHead $ filter openClosedPair $ zip reduced (tail reduced)
  where openClosedPair (a,b)=(elem a "([<{") && (elem b ")>}]")
        reduced = eliminateUntilStop s
        safeHead lst 
           | lst==[] = (' ', ' ') 
           | otherwise = head lst


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
  where reduced = eliminateUntilStop beg
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
