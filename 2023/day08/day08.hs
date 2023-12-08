module Day08 where 

import qualified Data.Map as M 
import Data.Char
import Data.Set (member)
import qualified Data.Set as S

data Direction = L | R deriving (Show, Read, Eq)
type Location = String
type Chart = M.Map Location (Location, Location)

parseDirections :: String -> [Direction]
parseDirections s = d
   where d = map (\x -> if x == 'L' then L else R) s 

parseLine :: String -> (Location, (Location, Location))
parseLine line = (label, (directions!!0, directions!!1))
    where (labelPart, otherPart) = break (=='=') line
          label = filter (/=' ') labelPart
          directions = words $ map (\c -> if isAlpha c then c else ' ') otherPart

parseLines :: [String] -> Chart
parseLines lines = M.fromList $ map parseLine lines

move :: Chart -> Location -> Direction -> Location
move chart loc d 
  | d == L = fst coord
  | d == R = snd coord
    where coord = M.findWithDefault ("A", "B") loc chart

moveUntilZZZ :: Chart -> Location -> [Direction] -> [Location]
moveUntilZZZ chart startLoc directionList = takeWhile (/="ZZZ") $ scanl (move chart) startLoc (cycle directionList)

part1 :: String -> IO()
part1 filename = do 
    contents <- lines <$> readFile filename
    let directions = parseDirections $ head contents 
    let chart = parseLines $ drop 2 contents 
    let trip = moveUntilZZZ chart "AAA" directions
    print $ length trip

endsWithA :: Location -> Bool
endsWithA "" = False
endsWithA s = (last s) == 'A'

endsWithZ :: Location -> Bool
endsWithZ "" = False
endsWithZ s = last s == 'Z'

moveUntilAllEndInZ :: Chart -> [Location] -> [Direction] -> [[Location]]
moveUntilAllEndInZ chart startLocs directionList = peel update
  where d = cycle directionList
        update = map (\startLoc -> scanl (move chart) startLoc d) startLocs
        peel lsts = let 
            firstElems = map head lsts
          in if (all endsWithZ firstElems) then [firstElems] else firstElems : peel (map tail lsts)
     

getStartingLocs :: Chart -> [Location]
getStartingLocs chart = filter (endsWithA) $ M.keys chart

takeUntilFirstDuplicate :: Ord a => [a] -> [a]
takeUntilFirstDuplicate xs = foldr go (const []) xs S.empty
  where
    go x cont set
      | member x set = [x]
      | otherwise = x : cont (S.insert x set)

    
findCycle :: Chart -> Location -> [Direction] -> [(Location, Int)]
findCycle chart startLoc directionList =  takeUntilFirstDuplicate pathAndDirection
    where path = scanl (move chart) startLoc (cycle directionList)
          pathAndDirection = zip path (cycle [0..(length directionList - 1)])

describeCycle :: [(Location, Int)] -> (Int, Int, [Int])
describeCycle cycle = (stepsBeforeCycleStarts, lengthOfCycle, locationOfZ)
    where lastElement = last cycle
          stepsBeforeCycleStarts = length $ takeWhile (/=lastElement) cycle
          lengthOfCycle = (length cycle) - stepsBeforeCycleStarts - 1
          locationOfZ = map snd $ filter (\(a, _) -> endsWithZ a) $ zip (map fst $ init cycle) [0..]


part2 :: String -> IO()
part2 filename = do 
    contents <- lines <$> readFile filename
    let directions = parseDirections $ head contents 
    let chart = parseLines $ drop 2 contents 
    let startLocs = getStartingLocs chart
    let f x = describeCycle $ findCycle chart x directions
    -- This only works because there is 1 "Z" entry in each orbit, which
    -- was checked manually.
    let cycleLengths = map (\(_,x,_) -> x) $ map f startLocs
    let myLCM = foldr lcm 1 cycleLengths
    print $ myLCM