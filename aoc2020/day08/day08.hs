module Day08 where 

data Cmd = NOP | JMP Int | ACC Int deriving (Show, Eq) 

parseCmd :: String -> Cmd 
parseCmd s 
  | c == "nop" = NOP
  | c == "jmp" = JMP val
  | c == "acc" = ACC val
  where (c, _:pv) = break (==' ') s 
        val = if (head pv=='+') then (read $ tail pv)::Int else (read pv)::Int

process :: Int -> Cmd -> Int 
process acc (ACC i) = acc + i 
process acc _ = acc

moveBy :: Cmd -> Int 
moveBy (JMP val) = val
moveBy _ = 1


