-- pe89
-- Roman numerals
-- Find the number of characters saved by writing each of these in their minimal form.
-- A text file contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals;
-- that is, they are arranged in descending units and obey the subtractive pair rule
-- (see http://projecteuler.net/about=roman_numerals for the definitive rules for this problem).
-- Answer: 

-- Converts a String containing a Roman numeral to an Int
numeralToInt :: String -> Int 
numeralToInt rn = sum $ foldl mergeLesser [] (map digits rn)
  where
    digits 'M' = 1000
    digits 'D' = 500
    digits 'C' = 100
    digits 'L' = 50
    digits 'X' = 10
    digits 'V' = 5
    digits 'I' = 1
    mergeLesser [] n = [n]
    mergeLesser s@(x:xs) n = 
      if (n <= x)
      then
        n:s
      else
        let h = n - (sum $ takeWhile (<n) s)
        in h:(dropWhile (<n) s)


-- Converts an Int into a Roman numeral (String) in minimal form
intToNumeral :: Int -> String          
intToNumeral n = show n            


-- Converts a valid Roman numeral into minimal form
minimal :: String -> String
minimal n = let v = numeralToInt n
            in show v


main = do
  input <- getContents
  let numerals = lines input
  print $ sum $ zipWith diff numerals (map minimal numerals)
     where diff n1 n2 = (length n1) - (length n2)
