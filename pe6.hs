-- Project Euler in Haskell problems 101..120

import ProjectEuler

-- 102
-- See pe6_102.hs


-- 112
-- Bouncy numbers: Find the least number for which the proportion of bouncy numbers is exactly 99%.
-- Answer: 
inc3 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9]] - 9
inc4 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9]] - 9 
inc5 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9], e <- [d..9]] - 9 
inc6 = length [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9], e <- [d..9], f <- [e..9]] - 9
dec3 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0]] - 9
dec4 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0]] - 9
dec5 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0], e <- [d,(d-1)..0]] - 9
dec6 = length [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0], e <- [d,(d-1)..0], f <- [e,(e-1)..0]] - 9
bouncy2 = 0
bouncy3 = 999 - 99 - inc3 - dec3 - 9 + bouncy2
bouncy4 = 9999 - 999 - inc4 - dec4 - 9 + bouncy3
bouncy5 = 99999 - 9999 - inc5 - dec5 - 9 + bouncy4
bouncy6 = 999999 - 99999 - inc6 - dec6 - 9 + bouncy5
nobounce = [[0,9,0],[36,9,45],[inc3,9,dec3],[inc4,9,dec4],[inc5,9,dec5],[inc6,9,dec6]]
-- There are 987048 bouncy numbers below 1,000,000 (~98.7%)
-- 1,000,000 is not bouncy, but the rest are to 1,100,000 then 1,110,000 then 1,111,000 then 1,111,100  then 1,111,110 to 1,111,119 are not bouncy
-- (987048 + x) / (1000000 + x) = 99/100  => x = 99,000,000 - 98,704,800 = 295200
-- Rats, we know there are only 14 non-bouncy in the next 111,119 numbers, but then it gets ugly.
