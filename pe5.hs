-- Project Euler in Haskell problems 81..100

import ProjectEuler


-- 92
-- Square digit chains: How many starting numbers below ten million will arrive at 89?
-- Answer: 8581146 (32.57 secs, 6717408512 bytes)
-- From testing, it appears that most of the time is spent in sum $ map (^2) (digits n)
-- The new solution involves using obvious solution for all the numbers below 567 = 7 * 9^2,
-- then building the sum of the squares of all the permutations of a 7 digit number, and checking for
-- inclusion in the list of solutions below 567.
-- only ~14% of the numbers converge to 1 (v. 86% for 89), based on the slow answer.
-- Through testing it was determined that is it 50%+ faster to check against the shorter list,
-- even though 85% of the numbers will search the entire list before failing.
-- I will be checking all numbers from 0000000 to 9999999, zero is not relevant, but it since
-- it is not in a chain that ends in 1, it won't be counted anyway.
shortList = filter (not . chain89) [1..567]
            where chain89 n
                      | n == 89   = True
                      | n == 1    = False
                      | otherwise = chain89 (sum $ map (^2) (digits n))
digitSums = [t | a <- sqs, b <- sqs, c <- sqs, d <- sqs, e <- sqs, f <- sqs, g <- sqs, let t = a + b + c + d + e + f+ g]
            where sqs = map (^2) [0..9]
pe92 = (10^7 - 1)  - (length $ filter chainEndsIn1 digitSums) where
    chainEndsIn1 n
      | n `elem` shortList  = True
      | otherwise           = False


-- 97
-- Large non-Mersenne prime: Find the last ten digits of 28433 * 2^7830457 + 1.
--    this is a massive non-Mersenne prime which contains 2,357,207 digits
-- Answer: 8739992577 (0.01 laptop secs, 1583960 bytes)
-- Note: (a  *b^c  +d) mod m == (a * (b^c mod m) + d) mod m 
-- we can solve b^c mod m with Modular exponentiation which is much more efficient when c is large.
pe97 = (28433 * (powerMod 2 7830457 (10^10)) + 1) `mod` 10^10


-- 99
-- See pe5_99.hs