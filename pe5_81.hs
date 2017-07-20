-- pe81
-- Path sum: two ways
-- Find the minimal path sum, in a text file containing a 80 by 80 matrix,
-- from the top left to the bottom right by only moving right and down.
-- Answer: 427337 (time: real	0m0.057s)
-- Analysis: add the bottom right to the two cells above and to the left.
-- repeat for each diagonal, where there are two options, take the smallest value.
-- this is similar to the triangle path sum in problem 18, where the triangle is on edge, and missing the corners


import ProjectEuler ( wordsWhen, zip3With)


--Stolen from problem 18 but modified to deal with squares and uses min instead of max
minMatSum :: (Num a, Ord a) => [[a]] -> a
minMatSum t = head $ foldr1 sumAdjacent t
  where sumAdjacent x y
           | length x < (length y) = zip3With minSum x y (tail y)
           | otherwise             = zip3With minSum x ((head y):y) (y ++ [last y])
        minSum x y z
           | y < z     = x + y
           | otherwise = x + z


-- make a list of diagonal f starting with the upper left from a matrix
makeDiagonals :: [[Int]] -> [[Int]]
makeDiagonals rows = foldl stepFunc seed (tail rows) where
                     seed = map (\x -> [x]) (head rows)
                     stepFunc acc ele = zipWith merge (acc ++ [[]]) ((replicate (1 + (length acc) - (length ele)) 0) ++ ele)
                     merge as b
                           | b  == 0   = as
                           | as == []  = [b]
                           | otherwise = b:as


-- make a list of lists (list of lines, where a line is a list of words, and each word is an int).
makeIntLists :: String -> [[Int]]
makeIntLists s = [map read (wordsWhen (== ',') x)::[Int] | x <- lines s]


main = do
  rows <- getContents
  print $ minMatSum $ makeDiagonals $ makeIntLists rows
