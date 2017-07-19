-- pe83
-- Path sum: four ways
-- Find the minimal path sum, in a text file containing a 80 by 80 matrix,
-- from the top left cell to the bottom right cell by moving up, down, left or right.
-- Answer:
-- compile with ghc pe5_83.hs; run with time ./pe5_83 < pe5_83matrix.txt

-- This is a little slower and more complicated than necessary, because it also keeps track of
--   the optimal path, for reporting/verification.

import ProjectEuler (wordsWhen, quicksort)
import Data.List (sort)
-- hide Preludes Left, Right as P.Left, P.Right
import Prelude hiding (Left, Right)
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
data Direction = Left| Right | Up | Down
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
  where size   = length (grid !! 0)


-- make a list of lists (list of lines, where a line is a list of words, and each word is an int).
makeGrid :: String -> Grid
makeGrid s = [map read (wordsWhen (== ',') x)::[Int] | x <- lines s]


main = do
  rows <- getContents
  let (value,index,path) = solve (makeGrid rows)
  putStrLn ("Minimum Path is " ++ show value ++ ".")
  putStrLn ("Start at row " ++ show index ++ " and proceed: " ++ show path)
