example = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"


data Mult = Mult Int Int deriving Show

parseString :: String -> String -> (Bool, String)
parseString token s = if startsWithToken then (True, drop (length token) s) else (False, drop 1 s)
  where startsWithToken = (take (length token) s) == token

parseMul :: String -> (Bool, String)
parseMul = parseString "mul"

parseOpenParen :: String -> (Bool, String)
parseOpenParen = parseString "("

parseCloseParen :: String -> (Bool, String)
parseCloseParen = parseString ")"

parseComma :: String -> (Bool, String)
parseComma = parseString ","

parseInt :: String -> (Int, String)
parseInt s = if length digits > 0 then (read digits, drop (length digits) s) else (0, drop 1 s)
    where digits = takeWhile (\c -> elem c "0123456789") s

parseMulOp :: String -> (Maybe Mult, String)
parseMulOp s = if hasMult && hasOpen && (lhs > 0) && hasComma && (rhs > 0) && hasClose then (Just (Mult lhs rhs), r6) else (Nothing, drop 1 s) 
  where (hasMult, r1) = parseMul s
        (hasOpen, r2) = parseOpenParen r1
        (lhs, r3) = parseInt r2
        (hasComma, r4) = parseComma r3
        (rhs, r5) = parseInt r4 
        (hasClose, r6) = parseCloseParen r5

parseAllMulOp :: String -> [Mult]
parseAllMulOp "" = []
parseAllMulOp s = case mm of 
        Just m -> m : parseAllMulOp rem
        Nothing -> parseAllMulOp rem
    where (mm, rem) = parseMulOp s


removeOffSections :: String -> String
removeOffSections s = case take 7 s of 
        "" -> ""
        "don't()" -> let rem = findDo $ drop 7 s in removeOffSections rem
        (c:_) -> c:(removeOffSections $ drop 1 s)
    where findDo "" = ""
          findDo x = if (take 4 x) == "do()" then drop 4 x else findDo $ drop 1 x


evalMult :: Mult -> Int 
evalMult (Mult a b) = a*b

part1 :: String -> IO ()
part1 filename = do
    mults <- parseAllMulOp <$> readFile filename
    let ans = sum $ map evalMult mults
    print(ans)

part2 :: String -> IO ()
part2 filename = do
    mults <- parseAllMulOp <$> removeOffSections <$> readFile filename
    let ans = sum $ map evalMult mults
    print(ans)