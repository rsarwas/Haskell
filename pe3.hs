-- Project Euler in Haskell problems 41..60

import ProjectEuler

-- 41
-- Pandigital prime: What is the largest n-digit pandigital (digits <- 1..n) prime that exists
-- Answer:


-- 42
-- See pe3_42.hs


-- 43
-- Substring divisibility: 
-- Answer:  16695334890  (0.02 laptop secs, 2154476 bytes)
-- seeds are digits $ takeWhile (<1000) (map (17*) [1..]) with the leading zeros preserved and those with duplicate digits removed 
pe43seeds = [[0,1,7],[0,3,4],[0,5,1],[0,6,8],[0,8,5],[1,0,2],[1,3,6],[1,5,3],[1,7,0],[1,8,7],[2,0,4],[2,3,8],
             [2,8,9],[3,0,6],[3,4,0],[3,5,7],[3,7,4],[3,9,1],[4,0,8],[4,2,5],[4,5,9],[4,7,6],[4,9,3],[5,1,0],
             [5,2,7],[5,6,1],[5,7,8],[6,1,2],[6,2,9],[6,8,0],[6,9,7],[7,1,4],[7,3,1],[7,4,8],[7,6,5],[7,8,2],
             [8,1,6],[8,5,0],[8,6,7],[9,0,1],[9,1,8],[9,3,5],[9,5,2],[9,8,6]]
pandigitals xs
  | null xs                 = xs
  | (length $ head xs) == 9 = [d:x | x <- xs, d <- [0..9], d `notElem` x] 
  | otherwise               = pandigitals [d:x | x <- xs, d <- [0..9], let divisor = primesTo 13 !! (8 - length x),
                                                                       d `notElem` x,
                                                                       (d*100 + (x!!0)*10 + x!!1) `mod` divisor == 0]
pe43 = sum $ map digitsToInt $ pandigitals pe43seeds


-- 44
-- Pentagon Numbers:
-- Answer:
pentagonNumbers = [(i,j,d `div`2) | i <- [1..10000], j <- [(i+1)..(i+1000)],
                    let ti = i*(3*i-1)
                        tj = j*(3*j-1)
                        a  = tj + ti
                        d  = tj - ti
                        sa = isqrt (1 + 12*a)
                        sd = isqrt (1 + 12*d),
                     sd*sd == 1 + 12*d,
                     (1 + sd) `mod` 6 == 0,
                     sa*sa == 1 + 12*a,
                     (1 + sa) `mod` 6 == 0]

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


-- 46
-- Goldbach's other conjecture
-- Answer: 5777 (0.93 laptop secs, 103750168 bytes)
oddComposites = filter (not . isPrime) [3,5..]
goldbachs = [(g,[(p,n) | n <- [1..isqrt((g-2) `div` 2)], let p = g -2*n*n, isPrime p]) | g <- oddComposites]
pe46 = head $ filter (null . snd) goldbachs


-- 48
-- Self Powers: Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-- Answer: 9110846700 (0.04 secs, 5809448 bytes)
pe48 = (sum [x^x | x <- [1..999]]) `mod` 10^10


-- 52
-- Permuted multiples: 
-- Answer: 142857 (1.49 laptop secs, 181350776 bytes)
-- In order for x and 6x to have the same number of digits, x must begin with 1
pe52domain = [136..166] ++ [1136..1666] ++ [11136..16666] ++ [111136..166666]
pe52test x = and [all ((n `elem`) . digits) [2*x, 3*x, 4*x, 5*x, 6*x]  | n <- digits x] 
pe52 = head $ filter pe52test pe52domain

