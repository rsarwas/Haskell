-- pe67
-- Maximum path sum II
-- Find the maximum total from top to bottom in triangle.txt, a 15K text file containing a triangle with one-hundred rows,
-- by starting at the top of the triangle below and moving to adjacent numbers on the row below.

-- Answer: 7273  (0.466 real laptop seconds; with unix time)

import ProjectEuler -- for Zip3With

--Stolen from problem 18 t is
maxTriSum :: (Num a, Ord a) => [[a]] -> a
maxTriSum t = head $ foldr1 sumAdjacent t
  where sumAdjacent x y = zip3With maxSum x y (tail y)
        maxSum x y z
               | y < z     = x + z
               | otherwise = x + y


-- make a list of lists (list of lines, where a line is a list of words, and each word is an int).
makeIntLists :: String -> [[Int]]
makeIntLists s = [map read (words x)::[Int] | x <- lines s]


main = do
  rows <- getContents
  print $ maxTriSum $ makeIntLists rows
