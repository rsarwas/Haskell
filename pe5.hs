-- Project Euler in Haskell problems 81..100

import ProjectEuler


-- 92
-- Square digit chains: How many starting numbers below ten million will arrive at 89?
-- Answer:
-- This could be made faster by keeping track of the numbers that led up to a 1 or 89,
-- and return true/false when I get to any one of those.  This would required a fast list searching routine.
-- Another trick would be generate a list of all the numbers below sum $ map (^2) (digits 9999999)
maxSum = sum $ map (^2) (digits 9999999) -- = 567 Which is a long list.
small89' = filter chain89 [1..maxSum]
small89'' = map (\n -> sum $ map (^2) (digits n)) small89'
percentBelow n = (100 * (length $ filter (<n) small89'')) `div` (length small89')
-- 90% of the numbers in small89' are below 115
small89 = filter chain89 [1..115]
small1 = filter (not . chain89) [1..115]
chain89 n
  | n == 89   = True
  | n == 1    = False
  | otherwise = chain89 (sum $ map (^2) (digits n))
chain89' n
  | n < 111 && n `elem` small89 = True
  | n < 111 && n `elem` small1  = False
  | otherwise        = chain89' (sum $ map (^2) (digits n))
pe92 = length $ filter chain89' [1..(10^7)]


-- 97
-- Large non-Mersenne prime: Find the last ten digits of 28433 * 2^7830457 + 1.
--    this is a massive non-Mersenne prime which contains 2,357,207 digits
-- Answer: 8739992577 (0.01 laptop secs, 1583960 bytes)
-- Note: (a  *b^c  +d) mod m == (a * (b^c mod m) + d) mod m 
-- we can solve b^c mod m with Modular exponentiation which is much more efficient when c is large.
pe97 = (28433 * (powerMod 2 7830457 (10^10)) + 1) `mod` 10^10