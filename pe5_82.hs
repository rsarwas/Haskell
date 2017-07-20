-- pe82
-- Path sum: three ways
-- Find the minimal path sum, in a text file containing a 80 by 80 matrix,
-- from any cell on the left edge to any cell on the right edge by only moving up, down and right.
-- Answer: 260324 (time real 0m0.138s)
-- This is a little slower and more complicated than necessary, because it also keeps track of
--   the optimal path, for reporting/verification.

import ProjectEuler (wordsWhen, quicksort)
-- hide Preludes Right as P.Right
import Prelude hiding (Right)
import qualified Prelude as P


-- Grid is m lists of n ints
-- Best is a list of m tuples for the last column in the grid.  The tuples
--   are in the form (cellValue, pathValue, path) where cellValue and pathValue are ints,
--   and path is defined below.  Best is calculated iteratively working from the right
--   edge (finish) to the left edge (start).  (to make list processing easier, we reverse
--   the grid, and work from head to tail).
-- Path is a list of Directions (Right, Up, Down) of the moves to make from the current
--   location to the right edge to get the minimal path value from the current cell.
--   note: Right is already defined in Prelude for Either, so we need to hide it as P.Right
-- Winner is a tuple of the minimum path value (Int), row index of the starting cell in the
--   left column (Int), and the path to the cell in the right column that makes the minimum
--   path value (Path).
type Grid = [[Int]]
data Direction = Right | Up | Down
     deriving (Eq, Ord, Show, Read, Bounded, Enum)
type Path = [Direction]
type Best = [(Int,Int,Path)]
type Winner = (Int, Int, Path)

-- Reverse the grid reverses each row (swaps left to right)
reverseGrid :: Grid -> Grid
reverseGrid grid = [reverse row | row <- grid]

-- trimGrid removes the left edge (creating a smaller grid to solve).
trimGrid :: Grid -> Grid
trimGrid grid = [xs | (_:xs) <- grid]

-- Do input checking, reverse the grid, create the default best value, and call the iteretive solver
solve :: Grid -> Winner
solve grid
  | null grid  = (0,-1,[])
  | size < 1   = (0,-1,[])
  | size == 1  = findWinner best'
  | otherwise  = findWinner (solve' grid'' best')
  where size   = length (grid !! 0)
        grid'  = reverseGrid grid
        grid'' = trimGrid grid'
        best'  = [(v1,0,[]) | (v1:_) <- grid']

findWinner :: Best -> Winner
findWinner best = minimum finalValuesWithIndex
  where finalValuesWithIndex = zipWith (\(v,path) i -> (v,i,path)) finalValues [1..]
        finalValues = map (\(v,pv,path) -> (v+pv,path)) best

-- Solve the grid by creating a best result recursively
-- There is no need to check for shifts up or down on the first or last columns (you would
--   just start or finish on the other row)
solve' :: Grid -> Best -> Best
solve' grid best
  | size == 1 = best'
  | otherwise = let best'' = shiftUpDown best'
                    grid'  = trimGrid grid
                in solve' grid' best''
  where size  = length (grid !! 0)
        best' = zipWith (\(v1, v2, path) (v:_) -> (v, v1+v2, Right:path)) best grid

-- Adjust the best path by adding additional segments up and down if appropriate
shiftUpDown :: Best -> Best
shiftUpDown best = map (\(i,b) -> findBetter i b best) $ zip [0..] best

-- Will return a new best path at index i if there is one.
findBetter :: Int -> (Int,Int,Path) -> Best -> (Int,Int,Path)
findBetter i (v,vp,p) best =
  let (uppers,downers) = splitAt i best
      betterUps = findBetterPath vp Up (reverse uppers)
      betterDowns = findBetterPath vp Down (if null downers then [] else tail downers)
      better =  quicksort (betterUps ++ betterDowns)
  in if null better then (v,vp,p) else (let (vp',p') = head better in (v,vp',p'))

-- returns a list of better options (lower path values than the current path, vp)
findBetterPath :: Int -> Direction -> Best -> [(Int,Path)]
findBetterPath vp direction others = map fix $ filter betterPath $ takeWhile possiblePath others''
   where fix (v',vp',p') = (v'+vp',p')
         betterPath (v',vp',_) = v'+ vp' < vp
         possiblePath (v',_,_) = v' < vp
         others' = scanl1 (\(v1,vp1,p1) (v2,vp2,p2) -> (v1+v2,vp2,p2)) others
         others'' = zipWith (\(v',vp',p') d -> (v',vp',d++p')) others' [replicate x direction | x <- [1..]]

-- make a list of lists (list of lines, where a line is a list of words, and each word is an int).
makeGrid :: String -> Grid
makeGrid s = [map read (wordsWhen (== ',') x)::[Int] | x <- lines s]


main = do
  rows <- getContents
  let (value,index,path) = solve (makeGrid rows)
  putStrLn ("Minimum Path is " ++ show value ++ ".")
  putStrLn ("Start at row " ++ show index ++ " and proceed: " ++ show path)
