module Party where

import Employee
import Data.Tree

-- Add an Employee to the GuestList
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL e f)= GL (e ++ [emp]) (f + (empFun emp))

-- nextLevel
-- computes the best fun score if we do invite the boss
-- and
-- computes the best fun score if we do not invite the boss
-- nextLevel Bob [....] -> (best guest list including Bob, best guest list without bob)
--      where [.....] are all the best guestlists under Bob
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel manager guestLists = (
  maximum $ concat $ map (\x -> [fst x, snd x]) guestLists,
  glCons manager glMax)
  where glMax = maximum $ map (fst) guestLists


extractRoot (Node rootLabel subForest) = rootLabel
extractSubtree (Node rootLabel subForest) = subForest

_maxFun :: Tree Employee -> (GuestList, GuestList)
_maxFun (Node _ []) = (GL [] 0, GL [] 0)
_maxFun t = nextLevel (extractRoot t) (map _maxFun $ extractSubtree t) 

maxFun :: Tree Employee -> GuestList
maxFun tree = maximum $ _maxFun tree

computeOutput :: Tree Employee -> String
computeOutput t  = "Total fun: " ++ (show fun) ++ "\n" ++ (unlines $ map (empName) lst)
                  where (GL lst fun) = maxFun t

main :: IO ()
main = readFile "company.txt" >>= putStrLn . computeOutput . read
