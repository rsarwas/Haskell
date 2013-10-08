-- Project Euler in Haskell problems 201..220

import ProjectEuler

-- 206
-- Concealed Square: Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0, where each “_” is a single digit.
-- Answer: 1389019170  (1.06 secs, 359285848 bytes)
-- Analysis: max = sqrt 192939... = 1.38e9; min = sqrt 102030... = 1.01e9. so  there are 10 digits,
--   the first digit is 1, and second digit is 0..3
--   Since the number ends in 0, the sqrt must end in zero, and the last x must also be zero.
--   the only squares that end in 9 are 3x3 = 9, and 7x7=49, so the penultimate digit is 3 or 7. 
--   if we let the digits of the solution be expressed as abcdefghij, then we can write out the
--   following expansion of terms (ignoring any term with j, since j == 0);
--   we know that a = 1, and b = [0..3], and look for c,d,e, that satisfy this equation
end  = [(g,h,i,j) | f <- [1], g <- [0..9], h <- [0..9], i <- [3,7], j <- [0],
        let t1 = (2*f*j + 2*g*i + h*h) `mod` 10,
        let t2 = (2*g*j + 2*h*i      ) `div` 10,
        let t3 = (  i*i              ) `div` 100,
            t1 <= 8,
            t2 <= 8 - t1, 
            t3 == 8 - t1 - t2]
end'  = [(e,f,g,h,i,j) | e <- [0..9], f <- [0..9], (g,h,i,j) <- end,
        let t1 = g*g + 2*e*i + 2*f*h,
        let t5 = (2*f*i + 2*g*h)* 100000,
        let t4 = (2*g*i + h*h)* 10000,
        let t3 = (2*h*i)* 1000,
        let t2 = (i*i)* 100,
            (t1 + ((t5+t4+t3+t2) `div` 100000)) `mod` 10 == 7]
trys = [(10+b)*10^8 +(cd*1000000) + (e*100000) + (f*10000) + (g*1000) + (h*100) + i*10 | b<-[0..3], cd <-[00..99], (e,f,g,h,i,j) <- end']
pe206 = head $ filter match trys
  where match n = match' (n*n `div` 100) 9
        match' _ 0 = True
        match' n d = if (n `mod` 10) == d then (match' (n `div` 100) (d-1)) else False
