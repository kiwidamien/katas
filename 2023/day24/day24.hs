module Day24 where 

type Loc2d = (Double, Double)
type Loc3d = (Int, Int, Int)


parseLine2D :: String -> (Loc2d, Loc2d)
parseLine2D s = let (pos, _:vel) = break (=='@') s 
                in (fn2 pos, fn2 vel)
  where fn2 part = let lst = map (read . filter (/=',')) $ words part in (lst!!0, lst!!1)

parseLine3D :: String -> (Loc3d, Loc3d)
parseLine3D s = let (pos, _:vel) = break (=='@') s 
                in (fn3 pos, fn3 vel)
  where fn3 part = let lst = map (read . filter (/=',')) $ words part in (lst!!0, lst!!1, lst!!2)


parse2D :: String -> [(Loc2d, Loc2d)]
parse2D contents = map parseLine2D $ lines contents 

parse3D :: String -> [(Loc3d, Loc3d)] 
parse3D contents = map parseLine3D $ lines contents 

-- Part 1

areParallel :: Loc2d -> Loc2d -> Bool 
areParallel (vx,vy) (ux,uy) =  (vx*uy - vy*ux) == 0


intersect :: (Loc2d, Loc2d) -> (Loc2d, Loc2d) -> Maybe Loc2d
intersect (p1@(x1,y1),v1@(vx1,vy1)) (p2@(x2,y2), v2@(vx2, vy2))
  | areParallel v1 v2 = Nothing
  | otherwise = if (t1 > 0) && (t2 > 0) then Just (x_pos, y_pos) else Nothing
    where num = vy1 * vx2 * x1 - vy2 * vx1 * x2 + vx1* vx2 * (y2-y1)
          denom = vy1 * vx2 - vy2 * vx1
          x_pos = num / denom
          y_pos = if vx1 == 0 then (vy1 * (x_pos - x1) + vx1 * y1)/vx1 else (vy2 * (x_pos - x2) + vx2*y2)/vx2
          t1 = if vx1 == 0 then (y_pos - y1) / vy1 else (x_pos - x1) / vx1 
          t2 = if vx2 == 0 then (y_pos - y2) / vy2 else (x_pos - x2) / vx2


intersectInArea :: (Loc2d, Loc2d) -> (Loc2d, Loc2d) -> Double -> Double -> Bool 
intersectInArea state1 state2 minXY maxXY = case intersect state1 state2 of 
    Nothing -> False
    Just (x,y) -> (minXY <= x) && (x <= maxXY) && (minXY <= y) && (y <= maxXY)

numIntersections :: [(Loc2d, Loc2d)] -> Double -> Double -> Int 
numIntersections [] _ _ = 0
numIntersections (state:rest) minXY maxXY = let 
    this_state = length $ filter (id) $ map (\b -> intersectInArea state b minXY maxXY) rest 
    in this_state + (numIntersections rest minXY maxXY)


part1 :: String -> IO ()
part1 filename = do
    contents <- parse2D <$> readFile filename 
    let nIntesections = numIntersections contents 200000000000000 400000000000000
    print (show nIntesections)

-- Part 2

{-
vx t + x = vx1 t + x1
vy t + y = vy1 t + y1
vz t + z = vz1 t + z1


-}