-- pe83
-- Path sum: four ways
-- Find the minimal path sum, in a text file containing a 80 by 80 matrix,
-- from the top left cell to the bottom right cell by moving up, down, left or right.
-- This is a shortest path problem, and the solution I've implemented is a
-- simple (naive) implementation of Dykstra's algorithm.  There are many ways
-- to improve my implementation, and certainly, the Haskell is somewhat crude,
-- but it is fast enough, and farily straight forward.
-- Answer: xxxxx in 0.672s unix time
-- compile with ghc -O2 pe5_83.hs; run with time ./pe5_83 < pe5_83matrix.txt

import ProjectEuler (wordsWhen)
import qualified Data.Map.Strict as Map

nrows :: NodeId
nrows = 80  -- Change to 3 for testGraph and 5 for pe5_83test.txt
ncolumns :: NodeId
ncolumns = nrows
finishId :: NodeId
finishId = nrows * ncolumns - 1  --zero based index
startId :: NodeId
startId = 0
maxCost :: Cost
maxCost = maxBound :: Int

data Status = Visited | Unvisited
     deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Graph = Map.Map NodeId Node
type NodeId = Int
type Cost = Int
type Node = (NodeId, Cost, Cost, Status)

dykstra :: (NodeId,Graph) -> Node
dykstra (n,g)
    | isFinish n = nodeAt n g
    | otherwise = dykstra (visit (n,g))

isFinish :: NodeId -> Bool
isFinish nid = nid == finishId

visit :: (NodeId,Graph) -> (NodeId,Graph)
visit (nid,graph) = (nid',graph')
    where
      n = nodeAt nid graph
      n' = makeVisited n
      (_,_,tc,_) = n
      neighbors = map (updateCost tc) (unvisitedNeighbors nid graph)
      graph' = insert (n':neighbors) graph
      nid' = smallestUnvisitedNode graph'

nodeAt :: NodeId -> Graph -> Node
nodeAt nid graph = graph Map.! nid

makeVisited :: Node -> Node
makeVisited (nid,c,tc,_) = (nid,c,tc,Visited)

updateCost :: Cost -> Node -> Node
updateCost c' (i,c,tc,s)
    | c + c' < tc = (i,c,c+c',s)
    | otherwise   = (i,c,tc,s)

unvisitedNeighbors :: NodeId -> Graph -> [Node]
unvisitedNeighbors n g =
  nodeAt' (r-1) c g ++ nodeAt' (r+1) c g ++
  nodeAt' r (c-1) g ++ nodeAt' r (c+1) g
  where
    (r,c) = n `quotRem` ncolumns

nodeAt' :: Int -> Int -> Graph -> [Node]
nodeAt' r c g
  | r < 0 || nrows <= r    = []
  | c < 0 || ncolumns <= c = []
  | v == Visited           = []
  | otherwise              = [n]
  where
    i = r * nrows + c
    n = nodeAt i g
    (_,_,_,v) = n

insert :: [Node] -> Graph -> Graph
insert []     g = g
insert (n:ns) g = insert ns (Map.insert i n g)
    where (i,_,_,_) = n

smallestUnvisitedNode :: Graph -> NodeId
smallestUnvisitedNode g = nid
  where
   (nid,_) = Map.foldl' min' (startId, maxCost) g
   min' (i,m) (nid',_,c,v) = if c < m && v == Unvisited then (nid',c) else (i,m)

makeGraph :: String -> Graph
makeGraph s = Map.fromList (firstNode:otherNodes)
  where
    firstCost = head costList
    firstNode = (0,(0,firstCost,firstCost,Unvisited))
    otherNodes = [(nid,(nid,cost,maxCost,Unvisited)) | (nid,cost) <- zip [1..] (tail costList)]
    costList = foldl1 (++) [map read (wordsWhen (== ',') x)::[Int] | x <- lines s]

testGraph :: Graph
testGraph =
  Map.fromList [(0,(0,1,1,Unvisited)),
                (1,(1,20,maxCost,Unvisited)),
                (2,(2,15,maxCost,Unvisited)),
                (3,(3,5,maxCost,Unvisited)),
                (4,(4,15,maxCost,Unvisited)),
                (5,(5,12,maxCost,Unvisited)),
                (6,(6,15,maxCost,Unvisited)),
                (7,(7,35,maxCost,Unvisited)),
                (8,(8,15,maxCost,Unvisited))]

main :: IO ()
main = do
  rows <- getContents
  let g = makeGraph rows
  let (_,_,pathSum,_) = dykstra (startId, g)
  putStrLn ("Minimum Path Sum is " ++ show pathSum)
