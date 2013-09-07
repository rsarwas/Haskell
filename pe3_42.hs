-- pe42
-- Coded triangle numbers
-- By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value.
-- If the word value is a triangle number then we shall call the word a triangle word.
-- Using words.txt containing nearly two-thousand common English words, how many are triangle words?
-- Answer: 162  (0.358 real laptop seconds; with unix time)

import ProjectEuler  -- for isqrt


isTriangleWord :: String -> Bool
isTriangleWord = isTriangleNumber . wordValue

-- tn = n(n+1)/2
-- 1n^2 + 1n + -2tn = 0 => n = (sqrt(1+8tn)  - 1)/2 by the quadratic formula
-- if n is an int, then tn is a triangle number, if n is an int, then 2n +1 is an int
-- therefore check that sqrt(1+8tn) is an int
isTriangleNumber :: Int -> Bool
isTriangleNumber n = let x  = 1 + 8 * n
                         sx = isqrt x
                     in sx*sx == x

-- score a word by adding the integer codes A = 1, B = 2, .. Z = 26 (all the words are in CAPS)
wordValue :: String -> Int
wordValue = sum . map ((subtract 64) . fromEnum)


-- make a list of words from the input file
makeWords :: String -> [String]
makeWords s = read ("[" ++ s ++ "]")


main = do
  input <- getContents
  print $ length $ filter isTriangleWord (makeWords input)
