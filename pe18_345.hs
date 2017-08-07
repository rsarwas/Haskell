-- pe345
-- Matrix Sum
-- Find the maximum sum of n elements in n x n matrix in a text file
-- Each element in the sum must be from a unique row and column.
-- That is only one element from each row and each column may participate in the sum.
-- Answer:
-- compile with ghc pe18_345.hs; run with time ./pe18_345 < pe345matrix.txt

-- This is nearly identical to the assignment problem
-- In the assignment problem, each row is a task, and each column is an actor,
-- and the values in the matrix are the cost for actor j to do task i.
-- This solution is based on the Hungarian solution to the assignment problem.

import ProjectEuler (transpose)

activateRows :: [[Int]] -> [[Int]]
activateRows = map activateRow

activateRows' :: [[Int]] -> [[Int]]
activateRows' = map activateRow'
  where activateRow' r = if   0 `elem` r
                         then r
                         else activateRow r

activateRow :: [Int] -> [Int]
activateRow r = map (subtract m) r
  -- where m = maximum r  -- Used if max is not converted to min
  where m = minimum r

activateColumns :: [[Int]] -> [[Int]]
activateColumns m = transpose $ activateRows' $ transpose m

-- first pass finds the max in each row,
-- and subtracts that value from each element in the row
-- This is repeated for each column (if there is no zero in the column)with that value, unless it has already
firstPass :: [[Int]] -> [[Int]]
firstPass m = activateColumns $ activateRows m

-- make a int matrix (list of list of ints), where a line is a list of words, and each word is an int.
makeMatrix :: String -> [[Int]]
makeMatrix s = [map read (words x)::[Int] | x <- lines s]

-- 'invert' the ints in a matrix
-- i.e.  subtract all cells from a 'large' value so that the max is the min
makeAssignment :: Int -> [[Int]] -> [[Int]]
makeAssignment n = map (map (\x -> n - x))

main:: IO ()
main = do
  rows <- getContents
  let m1 = makeMatrix rows
  let m2 = makeAssignment 1000 m1
  let m3 = firstPass m2
  print m1
  --print m2
  print m3
