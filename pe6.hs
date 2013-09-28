-- Project Euler in Haskell problems 101..120

import ProjectEuler

-- 102
-- See pe6_102.hs


-- 112
-- Bouncy numbers: Find the least number for which the proportion of bouncy numbers is exactly 99%.
-- Answer: 
inc3 = length $ [1 | a <- [1..9], b <- [a..9], c <- [b..9]]
inc4 = length $ [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9]]
inc5 = length $ [1 | a <- [1..9], b <- [a..9], c <- [b..9], d <- [c..9], e <- [d..9]]
dec3 = length $ [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0]]
dec4 = length $ [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0]]
dec5 = length $ [1 | a <- [9,8..1], b <- [a,(a-1)..0], c <- [b,(b-1)..0], d <- [c,(c-1)..0], e <- [d,(d-1)..0]]
bouncy2 = 0
bouncy3 = 999 - 99 - inc3 - dec3 + 9 + bouncy2
bouncy4 = 9999 - 999 - inc4 - dec4 + 9 + bouncy3
bouncy5 = 99999 - 9999 - inc5 - dec5 + 9 + bouncy4
