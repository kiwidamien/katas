module Day25 where 

import qualified Data.Map as M

type Name = String

parseGraph :: String -> M.Map Name [Name]
parseGraph s = M.fromList $ map parseLn myLines
    where myLines = lines s 
          parseLn ln = let lst = map (filter (/=':')) $ words ln in (head lst, tail lst)


exInput = "jqt: rhn xhk nvd\nrsh: frs pzl lsr\nxhk: hfx\ncmg: qnr nvd lhk bvb\nrhn: xhk bvb hfx\nbvb: xhk hfx\npzl: lsr hfx nvd\nqnr: nvd\nntq: jqt hfx bvb xhk\nnvd: lhk\nlsr: lhk\nrzs: qnr cmg lsr rsh\nfrs: qnr lhk lsr"

