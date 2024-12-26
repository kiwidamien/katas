import Data.Bits 

data OpCode = OpCode Int
data Operand = Zero | One | Two | Three | ValA | ValB | ValC | Err deriving (Show, Eq, Ord, Enum)
type Registers = (Int, Int, Int)

oct :: Int -> [Int]
oct n 
  | n < 8 = [n]
  | otherwise = let d = mod n 8 in oct (div (n - d) 8) ++ [d]


evalOperand :: Registers -> Operand -> Int
evalOperand _ Zero = 0
evalOperand _ One = 1
evalOperand _ Two = 2 
evalOperand _ Three = 3 
evalOperand (a,_,_) ValA = a 
evalOperand (_,b,_) ValB = b 
evalOperand (_,_,c) ValC = c 


adv :: Registers -> Operand -> Registers
adv (a,b,c) operand = (result, b, c)
  where result = div a (2 ^ (evalOperand (a,b,c) operand))

bxl :: Registers -> Operand -> Registers 
bxl (a,b,c) operand = (a, xor b $ fromEnum operand, c)

bst :: Registers -> Operand -> Registers
bst (a,b,c) operand = (a, mod (evalOperand (a,b,c) operand) 8, c)

bxc :: Registers -> Operand -> Registers 
bxc (a,b,c) _ = (a, xor b c, c)

out :: Registers -> Operand -> Registers 
out (a,b,c) operand = (a,b,c)

bdv :: Registers -> Operand -> Registers 
bdv (a,b,c) operand = (a, result, c)
  where result = div a (2 ^ (evalOperand (a,b,c) operand))

cdv :: Registers -> Operand -> Registers
cdv (a,b,c) operand = (a, b, result)
  where result = div a (2 ^ (evalOperand (a,b,c) operand))

jumpLoc :: Int -> Registers -> Operand -> Int 
jumpLoc pos (0,_,_) _ = pos + 2
jumpLoc pos _ op = fromEnum op


run :: [Int] -> Registers -> ([Int], Registers)
run program (a,b,c) = go program 0 (a,b,c) []
  where go :: [Int] -> Int -> Registers -> [Int] -> ([Int], Registers)
        go tape pos registers output
          | pos >= length tape = (output, registers)
          | instruction == 0 = go program (pos+2) (adv registers operand) output
          | instruction == 1 = go program (pos+2) (bxl registers operand) output
          | instruction == 2 = go program (pos+2) (bst registers operand) output 
          | instruction == 4 = go program (pos+2) (bxc registers operand) output 
          | instruction == 5 = go program (pos+2) (out registers operand) (output ++ [(mod (evalOperand registers operand) 8)])
          | instruction == 6 = go program (pos+2) (bdv registers operand) output
          | instruction == 7 = go program (pos+2) (cdv registers operand) output
          | instruction == 3 = go program (jumpLoc pos registers operand) registers output
          where instruction = program!!pos 
                operand :: Operand = toEnum $ program!!(pos + 1)

parse :: String -> (Registers, [Int])
parse s = ((readnum line0, readnum line1, readnum line2), readProg lineP)
  where r line =  drop 2 $ dropWhile (/=':') line
        readnum = read . r
        readProg line = map read $ words $ map (\c -> if c==',' then ' ' else c) $ r line
        (line0:line1:line2:_:lineP:_)=lines s

part1 :: String -> IO ()
part1 filename = do 
    (iniRegisters, program) <- parse <$> readFile filename 
    let (output, finalRegisters) = run program iniRegisters
    print(output)

part2 :: String -> IO ()
part2 filename = do 
    (_, program) <- parse <$> readFile filename 
    let start = product $ replicate 15 8
    let end = product $ replicate 16 8

    let iniState = head $ filter (\(output, _) -> output==program) $ map (\a -> run program (a, 0, 0)) [start..end] 
    print(iniState)
