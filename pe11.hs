-- Project Euler in Haskell problems 201..220

import ProjectEuler

-- 206
-- Concealed Square: Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0, where each “_” is a single digit.
-- Answer: 1389019170  (11.21 secs, 3612364408 bytes)
-- Analysis: max = sqrt 192939... = 1.38e9; min = sqrt 102030... = 1.01e9. so  there are 10 digits,
--   the first digit is 1, and second digit is 0..3
--   Since the number ends in 0, the sqrt must end in zero, and the last x must also be zero.
--   the only squares that end in 9 are 3x3 = 9, and 7x7=49, so the penultimate digit is 3 or 7. 
--   if we let the digits of the solution be expressed as abcdefghij, then the 2 at 10^16 is expressed by
--   2 = (2ac + b^2) mod 10 + (2ad + 2bc) div 10 + (2ae + 2bd + c^2) div 100
--   we know that a = 1, and b = [0..3], and look for c,d,e, that satisfy this equation
lead = [(a,b,c,d,e) | a <- [1], b <- [0..3], c <- [0..9], d <- [0..9], e <- [0..9],
        let t1 = (2*a*c +   b*b      ) `mod` 10,
        let t2 = (2*a*d + 2*b*c      ) `div` 10,
        let t3 = (2*a*e + 2*b*d + c*c) `div` 100,
            t1 <= 2,
            t2 <= 2 - t1,
            t3 == 2 - t1 - t2]
end  = [(g,h,i,j) | f <- [1], g <- [0..9], h <- [0..9], i <- [3,7], j <- [0],
        let t1 = (2*f*j + 2*g*i + h*h) `mod` 10,
        let t2 = (2*g*j + 2*h*i      ) `div` 10,
        let t3 = (  i*i              ) `div` 100,
            t1 <= 8,
            t2 <= 8 - t1, 
            t3 == 8 - t1 - t2]
-- trys = [((((((((10 + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f) * 10 + g) * 10 + h) * 10 + i) * 10 | (a,b,c,d,e) <- lead, f <- [0..9], (g,h,i,j) <- end]
-- turns out the lead numbers are not correctly collecting the carry bits, so we scrap that and use brute force on the middle numbers
trys = [(10+a)*10^8 +(b*10000) + (g*1000) + (h*100) + i*10 | a<-[0..3], b <-[0..9999], (g,h,i,j) <- end]
pe206 = head $ filter match trys
  where match n = match' (n*n `div` 100) 9
        match' _ 0 = True
        match' n d = if (n `mod` 10) == d then (match' (n `div` 100) (d-1)) else False
