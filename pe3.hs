-- Project Euler in Haskell problems 41..60

import ProjectEuler

-- 41
-- Pandigital prime: What is the largest n-digit pandigital (digits <- 1..n) prime that exists
-- Answer:


-- 42
-- See pe3_42.hs


-- 45
-- Triangular, pentagonal, and hexagonal: Given T285 = P165 = H143 = 40755, Find the next triangle number that is also pentagonal and hexagonal.
-- Answer: 1533776805; (tn, pn, hn, t=p=h) = (55385,31977,27693,1533776805) (0.44 laptop secs, 65336868 bytes)
-- h = hn(2hn-1) the hexagon number for hn
-- p = pn(3pn-1)/2 => 3pn^2 -1pn -2p = 0; if p = h then pn = (sqrt(1+24h) + 1)/6 by the quadratic formula
-- t = tn(tn+1)/2  => 1tn^2 +1tn -2t = 0; if t = h then tn = (sqrt(1+8h)  - 1)/2 by the quadratic formula
pe45 = head [((st-1) `div` 2, (sp+1) `div` 6, hn, h)
             | hn <- [144..],
               let  h = hn*(2*hn - 1),
               let st = (isqrt (1 + 8*h)),
               let sp = (isqrt (1 + 24*h)),
               all id [(st-1) `mod` 2 == 0, (sp+1) `mod` 6 == 0,
                       st*st == 1 + 8*h, sp*sp == 1 + 24*h]]


-- 48
-- Self Powers: Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-- Answer: 9110846700 (0.04 secs, 5809448 bytes)
pe48 = (sum [x^x | x <- [1..999]]) `mod` 10^10


-- 52
-- Permuted multiples: 
-- Answer:
