module Day20 where 

import qualified Data.Map as M 
import qualified Data.Set as S
import Debug.Trace

type Label = String
type Origin = String

data Switch = On | Off deriving (Show, Eq)
data ModuleType = FlipFlop Switch | Conjunction (M.Map String Pulse) | Broadcaster | Sink deriving (Show, Eq)
data LabeledModule = LM {_lmLabel:: Label, _lmType:: ModuleType} deriving Show
data Pulse = High Origin| Low Origin deriving (Show, Eq)

type ModuleBoard = M.Map Label (LabeledModule, [Label])

sflip :: Switch -> Switch 
sflip On = Off
sflip Off = On 

origin :: Pulse -> Origin
origin (High o) = o 
origin (Low o) = o

setOrigin :: Pulse -> String -> Pulse
setOrigin (High _) lab = High lab 
setOrigin (Low _) lab = Low lab 

isHigh :: Pulse -> Bool 
isHigh (High _) = True 
isHigh (Low _) = False 

send :: Pulse -> LabeledModule -> (Maybe Pulse, LabeledModule)
send (High _) (LM l (FlipFlop s)) = (Nothing, LM l (FlipFlop s))
send (Low _) (LM l (FlipFlop Off)) = (Just (High l), LM l (FlipFlop On))
send (Low _) (LM l (FlipFlop On)) = (Just (Low l), LM l (FlipFlop Off))
send pulse (LM l (Conjunction memory)) = (Just pulseType, LM l (Conjunction memory'))
  where memory' = M.insert (origin pulse) pulse memory
        pulseType = if all isHigh (M.elems memory') then (Low l)  else (High l)
send p (LM l Broadcaster) = (Just (setOrigin p l), (LM l Broadcaster))
send p (LM l Sink) = (Nothing, LM l Sink)



process :: Pulse -> Label -> ModuleBoard -> (ModuleBoard, [(Pulse, Label)], (Integer, Integer)) 
process pulse label board = trace (show newPulses) (board', newPulses, (low', high'))
  where (theModule, outputs) = case M.lookup label board of
            Just (m, outputs) -> (m, outputs) 
            Nothing -> error ("No module with label " ++ label)
        (mightPulse, theModule') = send pulse theModule
        board' = M.update (\(_, lst) -> Just (theModule', lst)) label board
        newPulses = case mightPulse of 
            Nothing -> []
            Just p' -> [(p', o) | o <- outputs]
        low' = fromIntegral $ length $ filter (not . isHigh . fst) newPulses
        high' = fromIntegral $ length $ filter (isHigh . fst) newPulses


parseModule :: String -> LabeledModule 
parseModule "broadcaster" = LM "broadcaster" Broadcaster 
parseModule ('%':rest) = LM rest (FlipFlop Off)
parseModule ('&':rest) = LM rest (Conjunction M.empty)


parseLine :: String -> (LabeledModule, [Label])
parseLine s = (parseModule firstWord, labels)
    where parts = words s
          firstWord = head parts
          labels = map (filter (/=',')) $ drop 2 parts


parse :: String -> ModuleBoard 
parse contents = foldl (\acc (lm, children) -> go acc (lm, children)) origBoard (M.elems origBoard) 
  where origBoard = memAndStaticBoard
        go :: ModuleBoard -> (LabeledModule, [String]) -> ModuleBoard
        go board (srcModule, children) = foldl (\acc c -> M.update (\x -> fn x) c board) board children
            where srcName = _lmLabel srcModule
                  fn (LM l' (Conjunction memory), others) = let memory'=M.insert srcName (Low srcName) memory in Just (LM l' (Conjunction memory'), others)
                  fn (lm, others) = Just (lm, others)
        memSetBoard = M.fromList $ map (\(m,labs) -> (_lmLabel m, (m, labs))) $ map parseLine $ lines contents
        unrefed = (S.difference) (S.fromList $ concat $ map snd $ M.elems memSetBoard) (S.fromList $ M.keys memSetBoard)
        memAndStaticBoard = foldl (\acc label -> M.insert label (LM label Sink, []) acc) memSetBoard unrefed
        

processUntilComplete :: ModuleBoard -> [(Pulse, Label)] -> (Integer, Integer) -> (ModuleBoard, (Integer, Integer))
processUntilComplete board [] (l', h') = (board, (l', h'))
processUntilComplete board ((p,l):rest) (l', h') = processUntilComplete board' (rest ++ newPulses) (l' + l'', h' + h'')
  where (board', newPulses, (l'', h'')) = process p l board 

pressButton :: ModuleBoard -> (ModuleBoard, (Integer, Integer))
pressButton board = processUntilComplete board [(Low "button", "broadcaster")] (1,0)

pressButtonMany :: ModuleBoard -> Integer -> (ModuleBoard, (Integer, Integer))
pressButtonMany board nTimes = go board nTimes (0,0)
  where go board 0 (l', h') = (board, (l', h'))
        go board n (l', h') = let (board', (l'', h'')) = pressButton board in go board' (n-1) (l' + l'', h' + h'')


part1 :: String -> IO ()
part1 filename = do 
    board <- parse <$> readFile filename
    let (_, (low, high)) = pressButtonMany board 1000
    print ((show low) ++ " x " ++ (show high) ++ " = " ++ (show $ low * high))


pressButtonUntilRepeat :: ModuleBoard -> (Integer, (Integer, Integer))
pressButtonUntilRepeat board' = undefined 
  where x = 4