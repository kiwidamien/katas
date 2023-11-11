module Day14 where

import Data.Map (Map)
import qualified Data.Map as M

type State = (Map Int Int, Int, Int)

initState :: [Int] -> State
initState lst = (M.fromList $ zip lst [0..], last lst, length lst) 

nextState :: State -> State
nextState (m, current, turnNum) = (M.insert n (turnNum+1) m, n, turnNum + 1)
  where n=5
  
