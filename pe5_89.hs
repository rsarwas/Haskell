-- pe89
-- Roman numerals
-- Find the number of characters saved by writing each of these in their minimal form.
-- A text file contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals;
-- that is, they are arranged in descending units and obey the subtractive pair rule
-- (see http://projecteuler.net/about=roman_numerals for the definitive rules for this problem).
-- Answer: 743  (time real 0m0.015s)
-- Reached level 3 (75 correct problems) only 11888 members (3.46%) have reached this level.  => 343584 members


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
intToNumeral n =
  let (m,r) = n `divMod` 1000
      (c,r') = r `divMod` 100
      (x,i) = r' `divMod` 10
      hundreds x
        | x <  4 = (replicate x 'C')
        | x == 4 = "CD"
        | x <  9 = 'D':(replicate (x-5) 'C')
        | x == 9 = "CM"
      tens x
        | x <  4 = (replicate x 'X')
        | x == 4 = "XL"
        | x <  9 = 'L':(replicate (x-5) 'X')
        | x == 9 = "XC"
      ones x
        | x <  4 = (replicate x 'I')
        | x == 4 = "IV"
        | x <  9 = 'V':(replicate (x-5) 'I')
        | x == 9 = "IX"
  in (replicate m 'M') ++ (hundreds c) ++ (tens x) ++ (ones i)


-- Converts a valid Roman numeral into minimal form
minimal :: String -> String
minimal n = let v = numeralToInt n
            in intToNumeral v


main = do
  input <- getContents
  let numerals = lines input
  print $ sum $ zipWith diff numerals (map minimal numerals)
     where diff n1 n2 = (length n1) - (length n2)
