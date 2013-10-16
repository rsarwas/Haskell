-- pe96
-- Sudoku
-- By solving all fifty puzzles in sudoku.txt find the sum of the 3-digit numbers found in
-- the top left corner of each solution grid.
-- Answer: 24702 time real 0m17.454s

-- Grid is 9 lists (rows) of 9 ints
type Grid = [[Int]]
type Cell = (Int,Int)  -- (row,column)

-- We know that all grids we are asked to solve will yield a solution,
-- but that is not true for our internal solver, so here we extract just the result from the Maybe
solveSudoku :: Grid -> Grid
solveSudoku puzzle = let (Just solution) = solveSudoku' puzzle
                     in solution

solveSudoku' :: Grid -> Maybe Grid
solveSudoku' puzzle
  | emptyCell == Nothing = Just puzzle
  | otherwise  = if (null solutions)
                 then Nothing
                 else head solutions
  where
    moves     = getMoves puzzle emptyCell
    solutions = filter (/=Nothing) [solveSudoku' (fillCell puzzle emptyCell move) | move <- moves]
    emptyCell = findCell puzzle

-- Find the best empty cell to consider.  Each iteration of the solver only has to consider
--  a single cell.  If none of the potential values in a cell work, then there is no solution.
-- for this naive solver, I will just pick the first empty cell
findCell :: Grid -> Maybe Cell
findCell puzzle = let cells = [(r,c) | r <- [0..8], c <- [0..8], let row = puzzle !! r, row !! c == 0]
                  in if null cells then Nothing else Just (head cells)

-- A list of values that a cell should try -- any value 1..9 that is not already in
-- play in the cells row, column or section.
getMoves :: Grid -> Maybe Cell -> [Int]
getMoves puzzle Nothing      = []
getMoves puzzle (Just (r,c)) = 
  let columnNumbers = filter (/= 0) $ [row !! c | row <- puzzle] 
      rowNumbers = filter (/= 0) $ (puzzle !! r)
      sectionNumbers = filter (/= 0) $ [(puzzle !! r') !! c' | r' <- [r''..(r''+2)], c' <- [c''..(c''+2)]] 
         where (r'',c'') = (3*(r `div` 3), 3*(c `div` 3))
      usedNumbers = columnNumbers ++ rowNumbers ++ sectionNumbers
  in [x | x <- [1..9], not (x `elem` usedNumbers)]
  

-- Returns a new puzzle grid with the given value replacing the value (empty cell) at row,column
fillCell :: Grid -> Maybe Cell -> Int -> Grid
fillCell puzzle (Just (r,c)) value =
  let initialRows = take r puzzle
      fixmeRow = puzzle !! r
      finalRows = drop (r+1) puzzle
      fixedRow = (take c fixmeRow) ++ (value:(drop (c+1) fixmeRow))
  in initialRows ++ (fixedRow:finalRows)


-- Get the sum of the top left 3 numbers in a Sudoku grid
topLeft3 :: Grid -> Int
topLeft3 grid = let r1 = head grid in 100 * (r1 !! 0) + 10 * (r1 !! 1) + (r1 !! 2)

-- make a list of grids (list of lists of Ints)
-- The input is 50 grids starting with "Grid xx" on a line, then 9 line of 9 digit numbers.
makeGrids :: String -> [Grid]
makeGrids s = makeGrids' [map (\c -> fromEnum c - 48) x | x <- lines s, take 4 x /= "Grid"]
makeGrids' :: [[Int]] -> [Grid]
makeGrids' [] = []
makeGrids' xs = let (h,t) = splitAt 9 xs
                in h:(makeGrids' t) 
main = do
  text <- getContents
--  print $ sum $ map (topLeft3 . solveSudoku) (makeGrids text) 
  print $ solveSudoku $ head (makeGrids text) 
