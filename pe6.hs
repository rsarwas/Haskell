-- Project Euler in Haskell problems 101..120

import ProjectEuler
import Data.Ratio
import Data.List (nub)

-- 102
-- See pe6_102.hs


-- 104
-- Pandigital Fibonacci ends
-- Answer: 329468 (13.06 new laptop secs, 8926902496 bytes)
pandigital19 n = length d == 9 && (not (0 `elem` d))
  where d = nub n
pandigitalTail n = pandigital19 $ digits $ n `mod` (10^9)
pandigitalHead (i, n) =
  let h = n `div` (10^(i `div` 5))
      d = take 9 $ digits h
  in  pandigital19 d
pe104 = fst $ head $ filter pandigitalHead $ filter (pandigitalTail . snd) $ zip [1..] fibs


-- 107
-- See pe6_107.hs

-- 112
-- Bouncy numbers: Find the least number for which the proportion of bouncy numbers is exactly 99%.
-- Answer: 1587000 (0.06 secs, 9492116 bytes) + lots of pencil and paper time.
inc3 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9]] - 9
inc4 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9]] - 9 
inc5 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9], e <- [d..9]] - 9 
inc6 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9], e <- [d..9], f <- [e..9]] - 9
inc7 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9], e <- [d..9], f <- [e..9], g <- [f..9]] - 9
dec3 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0]] - 9
dec4 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0]] - 9
dec5 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0], e <- [d,(d-1)..0]] - 9
dec6 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0], e <- [d,(d-1)..0], f <- [e,(e-1)..0]] - 9
dec7 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0], e <- [d,(d-1)..0], f <- [e,(e-1)..0], g <- [f,(f-1)..0]] - 9
bouncy2 = 0
bouncy3 = 999 - 99 - inc3 - dec3 - 9 + bouncy2
bouncy4 = 9999 - 999 - inc4 - dec4 - 9 + bouncy3
bouncy5 = 99999 - 9999 - inc5 - dec5 - 9 + bouncy4
bouncy6 = 999999 - 99999 - inc6 - dec6 - 9 + bouncy5
bouncy7 = 9999999 - 999999 - inc7 - dec7 - 9 + bouncy6
-- Is there a pattern in the results, look at the  count of all non-bouncy numbers: i.e: [monotonic increasing, steady,
-- and monotonically deacreasing] for each group, 1 digit, 2digit,.. 7digit
nobounce = [[0,9,0],[36,9,45],[inc3,9,dec3],[inc4,9,dec4],[inc5,9,dec5],[inc6,9,dec6],[inc7,9,dec7]]
-- nobounce => [[0,9,0],[36,9,45],[156,9,210],[486,9,705],[1278,9,1992],[2994,9,4995],[6426,9,11430]]
nobounceCount = sum $ concat nobounce -- == bouncy7
-- There are 987048 bouncy numbers below 1,000,000 (~98.7% -- too low) 
-- There are 9969183 bouncy numbers below 10,000,000 (~99.7% -- too high)
-- if all the numbers above 999999 were bouncy, then we would need 
plusUp = take 5 [(p,x) | x <- [1..], let n = (99 * (999999 + x)) % 100, let p = (numerator n) - 987048, (denominator n == 1), p < x]
-- plusUp => [(295200,295201),(295299,295301),(295398,295401),(295497,295501),(295596,295601)]
-- therefore with 1 no bounce number, % = 987048+295200/999999+295201 = 99/100; 
--   with 2 no bounce: % = 987048+295299/999999+295301 == 99/100; with 0 no bounce: 987048+295101/999999+295101 = 99/100;
--   answer = 1295100 + b*100 where b = number of no bounce numbers between 999999 and the answer
--   estimating about 2500 no bounce numbers in the next 295000 numbers, we will go to about 295000 + 100*2500  = 1,545,000

--   1000000 is not bouncy, but 1000001 to 1099999 are bouncy. 1100000, 1110000, 1111000 and 1111100 are not bouncy. 5 no bounce
--   1111101 - 109 bouncy, 110 - 19 no bounce, 122-29 nobounce ... 199 = 10+8+7+..+1 = 46
--   1111200 - 221 bouncy, 222 - 29 no bounce, 233-39 nobounce ... 299 =    8+7+..+1 = 36
--   1111300 - 332 bouncy, 333 - 39 no bounce, 344-49 nobounce ... 399 =    7+6+..+1 = 28
--   1111400 - 443 bouncy, 444 - 49 no bounce, 455-59 nobounce ... 499 =    6+5+..+1 = 21
--   ....
--   1111900 - 998 bouncy, 999 no bounce                                             = 1
--                                                             = 1+3+6+10+15+21+28+36+46 = 166
--   1112000 - 221 bouncy, 222 - 229 no bounce, 233-39 no bounce ... 299 = 8+7+..+1 = 36
--   1112300 - 332 bouncy, 333 - 339 no bounce, 344-49 no bounce ... 399 = 7+6+..+1 = 28
--   ...
--   1112900 - 998 bouncy, 999 no bounce								            = 1				 
--                                                                = 1+3+6+10+15+21+28+36 = 120
--   similarly, 11113000-3999 = 1+3+..+28 = 84; 4000-4999 = 56; 5000 - 35; 6000 - 20; 7000 - 10; 8000 - 4; 9000 - 1
--   total 1,000,000 to 1,119,999 = 5+166+120+84+56+35+20+10+4+1 = 501
--   1120000 to 1122221 bouncy, 222 - 229 no bounce; 233-239 no bounce ... 299  = 1+2+..+8 = 36
--   1122400 - 499 = sum [1..7] = 28  =>   1120000 -> 1129999 = 1+4+10+20+35+56+84+120 = 330
--   1130000 -1139999 (210), 40000 (126), 50000 (70) 60000 (35) 70000 (15) 80000 (5) 90000 (1)
--   total to 1199999 = 501 + 330 + 210 + 126 + 70 + 35 + 15 + 5 + 1 = 1293
--   1200000 to 1222221 bouncy; 222 - 229 no bounce, 233 - 239, 244-249, ... 299  = 8+7+..+1 = 36
--   by extrapolation from previous analysis, 1200000 - 1229999 = (36+28+..1)+(28+21+..1)+(21+15+..1)+..+(1) = 330
--                                       and  1200000 - 1299999 = 330 + 210 + 126 + 70 + 35 + 15 + 5 + 1 = 792 
--   similarly 1300000 - 1399999 = 210 + 126 + 70 + 35 + 15 + 5 + 1 = 462
--   similarly 1400000 - 1499999 = 126 + 70 + 35 + 15 + 5 + 1 = 252
--   total 1000000 - 1499999 = 1293 + 792 + 462 + 252 = 2799
--   new estimate of final answer = 1295100 + (2799)*100 = 1575000
--   if we add all the numbers 1500000 - 1599999  = 126 new no bouncy numbers => 1575000 + 12600 = 1587600 too low.
--   look at it more closely 1500000 - 1555554 bouncy 555-559 no bounce, 566-569 .. 599 = 1+2+3+4+5 = 15
--                           1555600 - 1555999  = (1+2+3+4) + (1+2+3) + (1+2) + (1) = 20  (35 5555 to 5999)
--                           1556000 - 1559999  = 20 + 10 + 4 + 1  (70 for 1500000 to 1560000)
--                           1560000 - 1566665 bouncy, 666 - 669 no bounce .. 699 = 1+2+3+4
--                           1560000 - 1569999 = 20 + 10 + 4 + 1 = 35
--                           1570000 - 1579999 = 10 + 4 + 1 = 15  (120 for 1500000 to 1580000)
-- 1575000 + 120*100 = 1587000
--   woo hoo, all the numbers from 1579999 to 1587000 are bouncy, so 1587000 is the answer


-- 114
-- Counting block combinations I:  How many ways can a row measuring fifty units in length be filled?
--   with block at least 3 units long, separated by at least 1 unit.
-- Answer: 
-- Analysis:  use a recursive solution.  Putting a block of length n at some point i in the 50 units will
--  yield 1 plus the solutions for the space prior plus the solutions for the empty space after.
-- countBlocks:
--  min is the smallest size group of units to consider
--  max is the largest size to consider (we only look for blocks less than or equal to
--       the starting block to avoid finding duplicate solutions.
--  len is the total number of units in the search space.
countBlocks :: Int -> Int -> Int -> Int
countBlocks min max len = sum [countforOneBlockOfSize n | n <- [min..max]]
  where
    -- problem need to subtract for symetrical equals,
    -- i.e: 4s3 <> 3s4 (correctly counted twice), however 3s3 = 3s3 (incorrectly counted twice)
    countforOneBlockOfSize l = sum [(blocksIn i) + 1 + (blocksIn (open-i)) | i <- [0..open]]
      where
        open = len - l
        blocksIn n = if (n < (min+1)) then 0 else (countBlocks min l (n-1))
pe114 = (countBlocks 3 50 50) + 1  -- plus one for the empty set.
-- unfortunately, in addition to being wrong, this solution is also exponetial in time
-- while it only took 0.18 sec for 25 units, and 45 seconds for 35 units, the prediction for 50 units is 9 hours


-- 116
-- Red, green or blue tiles : How many different ways can the black tiles in a row measuring
-- fifty units in length be replaced if colours cannot be mixed and at least one coloured tile must be used?
-- colours: red (length two), green (length three), or blue (length four).
-- Answer: 
-- The following recursive solution takes too long: fill 50 4 = 2.4 sec, fill 50 3 takes 59.5 sec, fill 50 2 is projected
-- to take 6 hours 
fill l n
  | l <  n    = 0
  | l == n    = 1
  | otherwise = 1 + (l-n) + (sum [fill x n | x <- [n..(l-n)]])
pe116 = (fill 50 2) + (fill 50 3) + (fill 50 4)

-- 120
-- Square Remainders: For 3 <= a <= 1000, find  rmax, where r is the remainder when (a-1)^n + (a+1)^n is divided by a^2
-- Answers: 333082500  (0.01 laptop secs, 4752528 bytes)
-- Analysis: by scribbling the expansion of (a+1), I found that (a+1)^n = x0a^n + x1a^(n-1) ... xna^0 where
--   x0, x1, ... xn is the nth row of pascals triangle, (a-1)n is the same with all the odd terms (x1, x3, ..) are negative.
--   when these terms are added, 2*(x0a^n + x1a^(n-1) ... xna^0) where the x are the nth row of pascals triangle,
--   but all the odd terms x1, x3, ... are 0. When n is even, all the terms except the last (2) are evenly divisible by a^2.
--   when n is odd, all the terms except the next to last (2*n*a) are evenly divisible by a^2.
--   therefore, the remainder = 2 (when n is even) or 2*a*n when n is odd.  Therefore we will look for odd n to find the
--   max r.  if n = a/2, then r = a^2 which = 0 mod a^2;  therefore rmax is when nmax is as close as possible to a/2.
--   This was verified by examining the using the computer to compute (a-1)^n + (a+1)^n `div` a^2 for the first
--   few a for n<- [0..20]
pe120 = sum [ 2*a*(n a) | a <- [3..1000]]
  where
    n a = if odd a then a `div` 2 else (a `div` 2) - 1