# Day 21: Parsers and DAGs


One of the common applications for Haskell is building up Parsers. 
The first part of day21, we are building a parser and a DAG to evaluate.
The second part is building a simple solver, which I will ignore for now.


I will be doing the part1 of the problem in 3 different ways

1. Doing it the basic way: take advantage of the highly regular format of the input to parse the content in a pretty brittle way.
   The hope is doing it this way will be "easier", because I don't have to support nested structures.
2. Building my own Parser.
   This is something that the final project of CIS-1940 got me used to, so it is good practice. I can also compare the amount of code to
   what is needed in the more brittle apprach.
3. Try using a parsing library (Parsec)
   Parsing seems formulaic enough that you can reduce boilerplate by using a library. Again, curious how much boilerplate from (2) this reduces, and 
   how this approach compares to (1).


## The problem

We are given a DAG, where the nodes are expressions:
```bash
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
```

To evaluate `root`, we would need to evaluate `pppw` and `sjmn`. We recursively do the evaluation:
```bash
root = pppw + sjmn = 2 + 150 = 152
         |     |
         |     ->  sjmn = drmz * dbpl = (30) * (5) = 150
         |                  |     |
         |                  |     -> 5
         |                  ---> hmdt - zczc = 32 - 2 = 30
         |                         |      |
         |                         -> 32  -> 2
         |
         -> pppw = cczh / lfqf  = 8 / 4 = 2
                    |      |
                    |      -> 4
                    -> cczh = sllz + lgvd = 4 + 4 = 8
                                |     |
                                |     -> lgvd = ljgn * ptdq = 2 * 2 = 4 
                                -> 4              |     |
                                                  -> 2  -> ptdq = humn - dvpt = 5 - 3 = 2
                                                                    |      |
                                                                    -> 5   -> 3
```

So there is the DAG evaluation part (similar across all three parts). The part that changes is the parsing of the input to the DAG.

## Evaluation (same across all)

This was the section that was common to all:
```haskell
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
```

The basic idea was that we would have `Map` (dictionary) from monkey names to expressions. In the input, we had to parse lines with format
```
<monkey name>: <string rep of an expression>
````
Here an expression was one of the following:
- a literal number (e.g. `5`)
- a name of another monkey (e.g. `gree`)
- a binary operation (on either numbers or monkeys), for example `5 + gree`, `groot / greep`, `8 * 5`

This inspired the `Expr` class, where an expression could be a literal number, a Monkey (i.e. string), or an operation of two subexpressions. Note the recurrsive definition allowed for more complex expressions than I had here. The data model allows things like
```
grrot: (snark + redd) + (5 - rupp)
```
as the parentheticals are also expressions! 

The evaluation would also take into account nested expressions. The "jungle" was passed in, so if the expression contained a monkey, we could use the jungle to look up that monkey and evaulate their expression.

THhroughout the code, we assumed the jungle formed a DAG, so evaluation had to terminate somewhere. This isn't really a simplifying assumption (we are not avoiding a more complex algorithm by doing this); if it was not a DAG then we would not have unique answers for evaluating the monkeys!

## Parsing

### Method 1: Brittle Parsing

We have to associate a string like `"5 + groot"` to the expression `Add (Lit 5) (Monkey "groot")`. The idea was to first use `words` to create a list and eliminate whitespace:
```haskell
words "5 + groot" = ["5","+","groot"]
```
From there, we could pattern match
```haskell
-- initial implementation, only +
strToExpr :: [String] -> Expr
strToExpr [e1,"+",e2] = Add (strToExpr [e1]) (strToExpr [e2])
```
In this case, `e1=5` and `e2="groot"`, which we also have to pattern match:
```haskell
-- adding support for basic (non-recursive) expressions, digits and strings
strToExpr :: [String] -> Expr
strToExpr [e1,"+",e2] = Add (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1]
  | (allDigits e1) = Lit (read e1::Integer)
  | otherwise = Monkey e1
```
This is enough to convert the string `5 + groot` into the expression `Add (Lit 5) (Monkey "groot")`. Once we have that, we can just eval the expression.

Adding in the other expressions was straight-forward. Here is the complete implementation:
```haskell
strToExpr :: [String] -> Expr
strToExpr [e1,"+",e2] = Add (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1,"-",e2] = Sub (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1,"*",e2] = Mul (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1,"/",e2] = Div (strToExpr [e1]) (strToExpr [e2])
strToExpr [e1] 
  | (allDigits e1) = Lit (read e1::Integer)
  | otherwise = Monkey e1
```
That's it for our parser!

The final piece is actually doing the calculation:
```haskell
parseContents :: [String] -> Map Monkey Expr
parseContents strings = Data.Map.fromList $ Data.List.map (parseLine) strings
    where parseLine line = (name, strToExpr $ words e)
              where [name, e] = splitOn ":" line


testCase = eval (Monkey "root") jungle
  where jungle = parseContents $ lines test


-- Evaluate the root monkey
main = do
  input <- readFile "day21_input.txt"
  let numCalled = eval (Monkey "root") $ parseContents $ lines input
```

#### The basic parser approach

