import qualified Data.Set as S 
import Data.List (nub)
import Debug.Trace (trace)


commaSplit :: String -> [String]
commaSplit [] = []
commaSplit s = let fstPart = takeWhile (\c -> c /= ',') s in fstPart:(commaSplit $ drop ((length fstPart) + 1) s)


interior :: [a] -> [a]
interior = init . tail 


parse_buttons :: String -> S.Set Int
parse_buttons strTuple = S.fromList $ map read $ commaSplit $ interior strTuple


parse :: String -> (S.Set Int, [S.Set Int], [Int])
parse line = (target, buttons, joltage)
    where parts = words line
          target = S.fromList [n | (n,c) <- zip [0..] $ interior (head parts), c=='#']
          pre_buttons = interior parts
          buttons = map parse_buttons pre_buttons
          joltage = map read $ commaSplit $ interior $ last parts


initial :: S.Set Int
initial = S.fromList [] 


press :: S.Set Int -> S.Set Int -> S.Set Int
press state button = S.union (state S.\\ button)  (button S.\\ state)

press' :: [Int] -> S.Set Int -> [Int]
press' state button = [if S.member position button then presses + 1 else presses | (position,presses) <- zip [0..] state]


bfs :: S.Set Int -> S.Set Int -> [S.Set Int] -> (S.Set Int, Int)
bfs start target options = go [(start, 0)] (S.fromList []) target options
   where go :: [(S.Set Int, Int)] -> S.Set (S.Set Int) -> S.Set Int -> [S.Set Int] -> (S.Set Int, Int)
         go [] seen target options = (S.fromList [], -1)
         go ((q, l):qs) seen target options
           | q == target = (q, l)
           | otherwise = let 
                qSet = S.fromList $ [qs' | (qs', _) <- qs]
                newQ = qs ++ (nub [(qq, l+1) | qq <- map (press q) options, S.notMember qq qSet, S.notMember qq seen])
                in go newQ (S.insert q seen) target options

notExceeds :: [Int] -> [Int] -> Bool
notExceeds changeCount targetChange = all (\(actual, limit) -> actual <= limit) $ zip changeCount targetChange


bfsTwo:: [Int] -> [Int] -> [S.Set Int] -> Int
bfsTwo start numPressTarget options = go [(start, 0)] numPressTarget options
  where go :: [([Int], Int)] -> [Int] -> [S.Set Int] -> Int
        go [] target options = -1
        go ((q, presses):qs) target options
          | q == target =  presses
          | otherwise = let 
               qSet = S.fromList $ [qs' | (qs', _) <- qs]
               newQ = qs ++ (nub [(qq, presses + 1) | qq <- map (press' q) options, notExceeds qq numPressTarget, S.notMember qq qSet])
               in trace (show q) $ go newQ numPressTarget options


solutionOne :: String -> (Int, [Int])
solutionOne contents = let 
    presses = map snd $ map (\(t,b) -> bfs initial t b) $ [(target, buttons) | (target, buttons, _) <- map parse $ lines contents]
    in (sum presses, presses)

solutionTwo :: String -> (Int, [Int])
solutionTwo contents = let
    presses = map (\(b,t) -> bfsTwo (take (length t) $ repeat 0) t b) $ [(buttons, target) | (_, buttons, target) <- map parse $ lines contents]
    in (sum presses, presses)


(_, buttons, joltage) = parse "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
counts = (take (length joltage) $ repeat 0)::[Int]