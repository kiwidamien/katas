module Day20pt2 where 

import qualified Data.Map as M
import Day20 (ModuleBoard, Pulse, LabeledModule, Label, parse, _lmLabel)

findInputs :: ModuleBoard -> Label -> [LabeledModule]
findInputs board label = map fst $ filter (\(_, others) -> elem label others) $ map snd $ M.assocs board

feed :: ModuleBoard -> [LabeledModule]
feed board = findInputs board "rx"

cycleOut :: ModuleBoard -> [LabeledModule]
cycleOut board = findInputs board (_lmLabel $ head $ feed board)

seen :: ModuleBoard -> M.Map Label Int
seen board = M.fromList $ zip (map (_lmLabel) $ cycleOut board) (repeat 0)