-- pe99
-- Largest exponential
-- Using a text file containing one thousand lines with a base/exponent pair on each line,
-- determine which line number has the greatest numerical value.
-- Answer: 709 (time: real	0m0.026s)
-- Analysis: if a^b < b^d then log(a^b) < log(b^d).   log(a^b) = b*log(a)

import ProjectEuler ( wordsWhen )


-- Calculate exp * log(base) for a line in the form "base,exp"
makeValue :: [Char] -> Double
makeValue line = let [b,e] = map (\s -> read s ::Double) $ wordsWhen (== ',') line
                 in log b * e

-- make a list of line vales and line number tuples from the input file
makePairs :: String -> [(Double, Int)]
makePairs s = let values = map makeValue (lines s)
              in zip values [1..]

main = do
  input <- getContents
  print $ snd $ maximum (makePairs input)
