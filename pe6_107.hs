-- pe107
-- Minimal Network: 
-- Answer:
-- Analysis:

import ProjectEuler (transpose)
--import Data.List (transpose)

-- make a list of lists (list of lines, where a line is a list of words, and each word is an int).
makeRows :: String -> [[Int]]
makeRows s = [read (fixLine line)::[Int] | line <- lines s]
  where fixLine s =  '[' :  (map (\x -> if x == '-' then '0' else x) s) ++ "]"


main = do
  text <- getContents
  let rows = makeRows text
      cols = transpose $ rows
      w    = (sum $ map sum rows) `div` 2
      wmin = sum [minimum $ filter (>0) col | col <- cols]
  putStrLn ("Original Weight is " ++ show w ++ ".")
  putStrLn ("Minimal Weight  is " ++ show wmin ++ ".")
  putStrLn ("Savings         is " ++ show (w - wmin) ++ ".")

