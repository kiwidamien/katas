import qualified Data.Array as A
import Day10 
import qualified Data.Set as S 
import qualified Data.Map.Strict as M
import Debug.Trace
import Data.List (foldl')
import Control.Monad.Trans.State.Strict

(target, buttons, joltage) = parse "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
counts = (take (length joltage) $ repeat 0)::[Int]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) (powerset xs) ++ powerset xs


pressesLightState :: [S.Set Int] -> S.Set Int
pressesLightState buttons = foldl (\a b -> (S.union a b) S.\\ (S.intersection a b)) start buttons
  where start::S.Set Int
        start = S.fromList []


count :: Ord a => [a] -> M.Map a Int
count xs = M.fromListWith (+) [(x, 1) | x <- xs]


pressLightToggleCount :: [S.Set Int] -> M.Map Int Int
pressLightToggleCount buttons = count $ concatMap S.toList buttons


memoStatesToButtonSeq :: [S.Set Int] -> M.Map (S.Set Int) [[S.Set Int]]
memoStatesToButtonSeq buttons =  M.fromListWith (++) $ map (\b -> (pressesLightState b, [b])) $ powerset buttons



reduceJoltage :: M.Map Int Int -> [Int] -> [Int]
reduceJoltage buttonsPressed originalJoltage = [j - M.findWithDefault 0 index buttonsPressed | (j, index) <- zip originalJoltage [0..]]


findMinimalSolution :: [S.Set Int] -> [Int] -> Int
findMinimalSolution buttons targetJoltage = go targetJoltage
    where pushMap = memoStatesToButtonSeq buttons
          go :: [Int] -> Int
          go js
            | all (==0) js = 0
            | any (<0) js = 10000000
            | all (even) js = 2*(go [div j 2 | j <- js])
            | otherwise = let
                simpleTarget = S.fromList [index | (indicator, index) <- zip (map (\x -> mod x 2) js)  [0::Int ..], indicator==1]
                options = M.findWithDefault [[]] simpleTarget pushMap
                timesLightsToggle = map pressLightToggleCount options
                newJoltages = map (\presses -> reduceJoltage presses js) timesLightsToggle
                debugList = [(length presses) + (go nj) | (presses, nj) <- zip options newJoltages]
                in if options == [] then 10000000 else minimum debugList


findMinimalSolution' :: [S.Set Int] -> [Int] -> Int
findMinimalSolution' buttons targetJoltage = fst $ go (M.empty) targetJoltage
    where pushMap = memoStatesToButtonSeq buttons
          go :: M.Map [Int] Int -> [Int] -> (Int, M.Map [Int] Int)
          go memo js = case M.lookup js memo of 
            Just v -> (v, memo)
            Nothing | all (==0) js -> (0, M.insert js 0 memo)
                    | any (<0) js -> (10000000, M.insert js 10000000 memo)
                    | all (even) js -> 
                        let
                            (v', memo') = (go memo [ j `div` 2 | j <- js])
                            !v = 2*v' 
                        in (v, M.insert js v memo')
                    | otherwise -> let
                        simpleTarget = S.fromList [index | (indicator, index) <- zip (map (\x -> mod x 2) js)  [0::Int ..], indicator==1]
                        options = M.findWithDefault [[]] simpleTarget pushMap
                        timesLightsToggle = map pressLightToggleCount options
                        newJoltages = map (\presses -> reduceJoltage presses js) timesLightsToggle
                        --debugList = [(length presses) + (go memo nj) | (presses, nj) <- zip options newJoltages]
                        (best, memo') = foldl' (\(bestSoFar, m) option -> let
                            presses = pressLightToggleCount option
                            nj = reduceJoltage presses js
                            (cost, m') = go m nj
                            !total = length presses + cost
                            in (min bestSoFar total, m')) (10000000, memo) options
                    in (best, M.insert js best memo')


findMinimalSolution'' :: [S.Set Int] -> [Int] -> Int
findMinimalSolution'' buttons targetJoltage = go targetJoltage
  where 
    pushMap = memoStatesToButtonSeq buttons
    memo :: M.Map [Int] Int
    memo = M.fromListWith const [(js, value js)| js <- []]
    lookupMemo :: [Int] -> Int
    lookupMemo js = case M.lookup js memo of 
        Just v -> v
        Nothing  ->
            let v = value js
            in v
    value :: [Int] -> Int 
    value js
      | all (==0) js = 0
      | any (<0) js = 10000000
      | all even js = 2 * lookupMemo [j `div` 2 | j <- js]
      | otherwise = 
          let simpleTarget = S.fromList [i | (ind, i) <- zip (map (`mod` 2) js) [0..], ind==1]
              options = M.findWithDefault [[]] simpleTarget pushMap
              totals = [ length presses + lookupMemo (reduceJoltage (pressLightToggleCount presses) js)
                       | presses <- options ]
          in if null totals then 10000000 else minimum totals

    go::[Int] -> Int
    go = lookupMemo

findMinimalSolution''' :: [S.Set Int] -> [Int] -> Int
findMinimalSolution''' buttons target =
  evalState (go target) M.empty
  where
    pushMap = memoStatesToButtonSeq buttons
    inf = 10000000

    go :: [Int] -> State (M.Map [Int] Int) Int
    go js = do
      memo <- get
      case M.lookup js memo of
        Just v ->
          pure v
        Nothing -> do
          v <- compute js
          modify' (M.insert js v)
          pure v

    compute :: [Int] -> State (M.Map [Int] Int) Int
    compute js
      | all (==0) js =
          pure 0
      | any (<0) js =
          pure inf
      | all even js = do
          v <- go [j `div` 2 | j <- js]
          pure (2 * v)
      | otherwise = do
          let simpleTarget =
                S.fromList
                  [ i
                  | (ind, i) <- zip (map (`mod` 2) js) [0..]
                  , ind == 1
                  ]

              options =
                M.findWithDefault [] simpleTarget pushMap
                -- options :: [[S.Set Int]]

          costs <- mapM (costFor js) options
          pure (if null costs then inf else minimum costs)

    costFor :: [Int] -> [S.Set Int] -> State (M.Map [Int] Int) Int
    costFor js pressesSets = do
      let presses = pressLightToggleCount pressesSets
          nj      = reduceJoltage presses js
      v <- go nj
      pure (length presses + v)

solTwo :: String -> [Int]
solTwo contents = map (\(_, buttons, joltage) -> findMinimalSolution''' buttons joltage) $ map parse $ lines contents

partTwo :: String -> IO ()
partTwo filename = do 
    contents <- readFile filename
    let result = solTwo contents
    print (sum $ result)