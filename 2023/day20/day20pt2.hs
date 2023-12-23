module Day20pt2 where 

import qualified Data.Map as M
import Day20 (ModuleBoard, Pulse(..), LabeledModule, Label, parse, _lmLabel, process, isHigh, origin)
import Debug.Trace 


findInputs :: ModuleBoard -> Label -> [LabeledModule]
findInputs board label = map fst $ filter (\(_, others) -> elem label others) $ map snd $ M.assocs board

feed :: ModuleBoard -> [LabeledModule]
feed board = findInputs board "rx"

cycleOut :: ModuleBoard -> [LabeledModule]
cycleOut board = findInputs board (_lmLabel $ head $ feed board)

initializeCycleCount :: ModuleBoard -> M.Map Label Int
initializeCycleCount board = M.fromList $ zip (map (_lmLabel) $ cycleOut board) (repeat 0)


processBtnPush :: ModuleBoard -> [(Pulse, Label)] -> (ModuleBoard, [Label])
processBtnPush board pulses = go' board [] pulses
  where feedLabel = _lmLabel $ head $ feed $ board
        tracked = map _lmLabel $ cycleOut board
        go' board acc [] = (board, acc)
        go' board acc ((p,l):rest) = go' board' acc' (rest ++ newPulses)
           where (board', newPulses, _) = process p l board
                 acc' = if (l==feedLabel) && (isHigh p) then ((origin p):acc) else acc


pressButton :: ModuleBoard -> (ModuleBoard, [Label])
pressButton board = processBtnPush board [(Low "button", "broadcaster")]

countCycles :: ModuleBoard -> M.Map Label Int 
countCycles board = go 0 board initialCount 
  where initialCount = initializeCycleCount board
        go :: Int -> ModuleBoard -> M.Map Label Int -> M.Map Label Int
        go presses board counter
          | all (/= 0) (M.elems counter) = counter
          | otherwise = let 
                          (board', pulses) = pressButton board
                          counter' = foldl (\acc label -> M.insertWith (\old new -> if old == 0 then new else old) label presses acc) counter pulses
                        in go (presses + 1) board' counter'


lcmSingle :: Int -> Int -> Int 
lcmSingle m n = div (m * n) (gcd m n)

lcmMany :: [Int] -> Int
lcmMany nums = trace (show nums) $ foldl (\acc n -> lcmSingle acc n) 1 $ map (+1) nums

part2 :: String -> IO () 
part2 filename = do 
    board <- parse <$> readFile filename 
    let cycles = countCycles board
    print (cycles)
    let lcmPush = lcmMany $ M.elems cycles 
    print(lcmPush)

testBoard = parse <$> readFile "input.txt"
btnResult = pressButton <$> testBoard