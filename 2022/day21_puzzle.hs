import Data.Map 
import Data.Char
import Data.List
import Data.List.Split

type Monkey = String
type Jungle = Map Monkey Expr
--test = "wnqf: pnwq * htpn\npnwq: 45\nhtpn: 2"
test = "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\n" ++ "dvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\n" ++ "pppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32"

data Expr = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Lit Integer 
  | Monkey Monkey  deriving (Eq, Show, Ord)


allDigits :: String -> Bool
allDigits s = Data.List.foldr (&&) True (fmap isDigit s)


eval :: Expr -> Jungle -> Integer
eval (Monkey m) jungle = eval (jungle!m) jungle
eval (Lit num) _ = num
eval (Add e1 e2) j = (+) (eval e1 j) (eval e2 j)
eval (Sub e1 e2) j = (-) (eval e1 j) (eval e2 j)
eval (Mul e1 e2) j = (*) (eval e1 j) (eval e2 j)
eval (Div e1 e2) j = (div) (eval e1 j) (eval e2 j)

strToExpr :: [String] -> Expr
strToExpr [e1,"+",e2] = Add (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1,"-",e2] = Sub (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1,"*",e2] = Mul (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1,"/",e2] = Div (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1] 
  | (allDigits e1) = Lit (read e1::Integer)
  | otherwise = Monkey e1


parseContents :: [String] -> Map Monkey Expr
parseContents strings = Data.Map.fromList $ Data.List.map (parseLine) strings
    where parseLine line = (name, strToExpr $ words e)
              where [name, e] = splitOn ":" line


testCase = eval (Monkey "root") jungle
  where jungle = parseContents $ lines test


main = do
  input <- readFile "day21_input.txt"
  let numCalled = eval (Monkey "root") $ parseContents $ lines input
  print numCalled
