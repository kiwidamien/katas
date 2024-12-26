import Data.Bits
import qualified Data.Map as M 
import Data.List (sort, unfoldr)


data Op = AND | XOR | OR deriving (Show, Eq)
data Expression = Literal Int | Compound Op Expression Expression | Reference String deriving (Show, Eq)
data Assignment = Assignment String Expression deriving (Show, Eq)
type Register = M.Map String Expression
data FailMode = XFail | YFail | Both | NoFailure deriving (Show, Eq) 


-- Housekeeping
opFromName :: String -> Op 
opFromName "AND" = AND 
opFromName "OR" = OR 
opFromName "XOR" = XOR 

getTarget :: Assignment -> String 
getTarget (Assignment target _) = target 

getExpression :: Assignment -> Expression
getExpression (Assignment _ e) = e 

-- Parsing
parseInitial :: String -> (String, Int)
parseInitial line = (takeWhile (/=':') line, read $ head $ drop 1 $ words line)

parseAssignment :: String -> Assignment
parseAssignment assignment = Assignment target expression
  where [name1, opName, name2,_,target] = words assignment
        expression = Compound (opFromName opName) (Reference name1) (Reference name2)

parse :: String -> Register
parse contents = M.fromList (initialAssignments ++ rules)
  where initialAssignments = map (\(target, value) -> (target, Literal value)) $ map parseInitial $ takeWhile (/="") $ lines contents
        rules = map (\x -> (getTarget x, getExpression x)) $ map parseAssignment $ drop 1 $ dropWhile (/="") $ lines contents

-- General Evaluation
evaluate :: Register -> Expression -> Int
evaluate _ (Literal n) = n
evaluate r (Compound AND e1 e2) = (evaluate r e1) .&. (evaluate r e2)
evaluate r (Compound OR e1 e2) = (evaluate r e1) .|. (evaluate r e2)
evaluate r (Compound XOR e1 e2) = xor (evaluate r e1) (evaluate r e2)
evaluate r (Reference target) = case M.lookup target r of 
    Just exp -> evaluate r exp

evalZBits :: Register  -> [Int]
evalZBits rulebook = map (\n -> evaluate rulebook (Reference n)) $ reverse $ sort $ filter (\name -> head name == 'z') $ M.keys rulebook

evalZBitsToInt :: Register -> Int 
evalZBitsToInt rulebook = foldl (\acc new -> 2*acc + new) 0 bits 
  where bits = evalZBits rulebook


part1 :: String -> IO ()
part1 filename = do 
    rulebook <- parse <$> readFile filename
    let finalNumber = evalZBitsToInt rulebook
    print(finalNumber)

toReverseBits :: Int -> [Int]
toReverseBits 0 = [0]
toReverseBits number = unfoldr (\n -> if n==0 then Nothing else Just (mod n 2, div n 2)) number

-- Network Checking for Adder 

setXandY :: Register -> Int -> Int -> Register
setXandY rulebook x y = M.union (M.fromList (xBitSet ++ yBitSet)) rulebook
  where xRegisters = sort $ filter (\n -> head n == 'x') $ M.keys rulebook 
        yRegisters = sort $ filter (\n -> head n == 'y') $ M.keys rulebook
        xBitSet = zip xRegisters (map (Literal) $ (toReverseBits x) ++ repeat 0)
        yBitSet = zip yRegisters (map (Literal) $ (toReverseBits y) ++ repeat 0)


checkNthBit :: Register -> Int -> FailMode
checkNthBit rulebook bitNum
  | justX /= (2^bitNum) = XFail 
  | justY /= (2^bitNum) = YFail
  | bothXY /= (2^bitNum)*2 = Both
  | otherwise = NoFailure
  where justX = evalZBitsToInt $ setXandY rulebook (2^bitNum) 0
        justY = evalZBitsToInt $ setXandY rulebook 0 (2^bitNum)
        bothXY = evalZBitsToInt $ setXandY rulebook (2^bitNum) (2^bitNum)

check :: Register -> [(Int, FailMode)]
check rulebook = [(bitNum, checkNthBit rulebook bitNum) | bitNum <- [0..maxBits], (checkNthBit rulebook bitNum /= NoFailure)]
  where maxBits = (length $ filter (\n -> head n == 'x') $ M.keys rulebook) - 1

swaps :: Register -> [(String, String)] -> Register 
swaps r swaps = M.fromList $ map (\(key, value) -> (f key, value)) $ M.toList r 
  where rDict = M.fromList (swaps ++ map (\(f,s) -> (s, f)) swaps) 
        f :: String -> String 
        f s = M.findWithDefault s s rDict 
        swapExpression :: Expression -> Expression
        swapExpression (Literal n) = Literal n 
        swapExpression (Reference s) = Reference (f s)
        swapExpression (Compound o e1 e2) = Compound o (swapExpression e1) (swapExpression e2)

part2 :: String -> IO ()
part2 filename = do 
    rulebook <- parse <$> readFile filename
    let swapNodes = [("z10", "gpr"), ("z21", "nks"), ("z33", "ghp"), ("cpm", "krs")]
    let newRules = swaps rulebook swapNodes
    let failedBits = check newRules
    let nodesThatSwapped = map (\c -> if c==' ' then ',' else c) $ unwords $ sort $ concat $ map (\(x,y) -> [x,y]) swapNodes
    print(failedBits)
    print(check rulebook)
    print(nodesThatSwapped)
