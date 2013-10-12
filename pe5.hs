-- Project Euler in Haskell problems 81..100

import ProjectEuler
import Data.Set (fromList, size)  --for uniquing in 87

-- 81
-- See pe5_81.hs


-- 82
-- See pe5_82.hs


-- 85
-- Counting rectangles: Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution.
-- Answer: 2772 (0.00 secs, 2059720 bytes)
-- Analysis: a grid that is 1xn units, has 1 rect 1xn, 2 rects 1x(n-1).  ... n rects (1x1)
--   therefore, there are 1+2+...+n rectangles, this is the nth triangle number t(n) = n(n+1)/2
--   If the grid is 1 unit high, there is 1 collection with t(n), if the grid is 2 units high,
--   there is 1 collection 2 units high, and 2 collections 1 unit high, or three total collections
--   at this point it should be clear that there are t(m) collections in a grid m units high.
--   therefore if the grid is n x m, there are t(n)*t(m) = n(n+1)m(m+1)/2/2 rectangles
--   testing this with the 2x3 grid in the problem statement, we get 18 rectangles as shown.
--   therefore, we are looking for m and n which minimize abs(2000000 - n(n+1)m(m+1)/2/2).
--   at one extreme, m = 1 (or n = 1), and n(n+1) = sqrt 4000000 = 2000, so n = 1990 or 2000,
--   1x1999 = 1999000 rectangles, and 1x2000 = 2001000 rectangle, both 1000 rectangles away.
--   assuming that there is an unambiguous best answer, the long skinny solution is not the answer.
--   at the other extreme, the grid is square (or nearly so) n ~ 4th root of 8000000 ~ 53.183
--   53x53 = 2047761, or 47761 over, much further from the goal.
--   if we have a width n, then the height m would be m(m+1) = 2 * 2 * 2000000 / n / (n+1)
--   therefore, the if low = sqrt (2 * 2 * 2000000 / n / (n+1), the two options are low-0.5 and low+0.5
--   if we look at the grids 1 to 53 high, we can calculate the two lengths on both sides of 2000000
rectCount n m = n*(n+1)*m*(m+1) `div` 4
widthOptions h = let h' = fromIntegral h in let low = truncate $ sqrt ((8000000 / h' / (h'+1)) - 0.5) in (low,low+1)
rects = foldl (\rects h -> let (w1,w2) = widthOptions h in [h,w1]:[h,w2]:rects) [] [1..53]
closest = minimum $ map (\[h,w] -> [(abs (2000000 - (rectCount w h))),h,w]) rects
pe85 = let [diff,h,w] = closest in w*h


-- 87
-- Prime power triples: How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
-- Answer: 1097343 (8.46 secs, 1412321004 bytes)
-- Analysis: Check all prime upto to the fourth root of 50e6 for the fourth power term.  With each of those,
--   check all primes up to the cube root of remainder, continue in this way to the square root.
--   The biggest speed up comes from precomputing a single list of primes that are filtered as necessary.
--   This will create some duplicates, which must found and removed;
--   This brute force solution takes about 1 seconds to create over one million numbers,
--   and then an additional 56 seconds to remove about 40,000 non-unique solutions.
--   There are much better uniquing solutions using hash tables.
--   There might be a trick to eliminate duplicates before adding to the list (i.e. does it help to know 4^2 = 2^4)
primePowerTriples n = [p | c <- takeWhile (<=(max4 n)) ps, let c4 = c^4, b <- takeWhile (<=(max3 (n-c4))) ps, let b3 = b^3, a <- takeWhile (<=(max2 (n-c4-b3))) ps, let p = a^2 + b3 + c4]  -- 1.3 sec (but 338 seconds to quicksort - out of order)
   where ps = primesTo (max2 n)
         max2 x = floor ((fromIntegral x)**(1/2))
         max3 x = floor ((fromIntegral x)**(1/3))
         max4 x = floor ((fromIntegral x)**(1/4))

pe87 = size $ fromList $ primePowerTriples 50000000
--pe87 = length $ unique $ quicksort $ primePowerTriples 50000000
--pe87 = length $ primePowerTriples 50000000


-- 89
-- See pe5_89.hs


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