-- Project Euler in Haskell problems 201..220

import ProjectEuler
import Data.Ratio
import Data.List

-- 204
-- Generalised Hamming Numbers: How many generalised Hamming numbers of type 100 are there which don't exceed 10^9?
-- Answer: 2944730 (47.51 secs, 16,828,030,440 bytes)
-- A Hamming number is a positive number which has no prime factor larger than 5.
-- We will call a positive number a generalised Hamming number of type n, if it has no prime factor larger than n.
-- There are 1105 Hamming numbers of type 5 not exceeding 10^8.
-- Analysis: Hamming numbers of type 5 not exceeding 100
--           = [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36,40,45,48,50,54,60,64,72,75,80,81,90,96,100]
--           = [1,5,25, 3,15,75,9,45,27,81, 2,10,50,6,30,18,90,54,4,20,100,12,60,36,8,40,24,72,16,80,48,32,96,64]
--           = [1,5,25] x [1,3,9,27,81] x [1,2,4,8,16,32,64]

-- The powers of a number x not exceeding 10^n
-- powers 3 2 -> [1,3,9,27,81]
powers x n = takeWhile (<=(10^n)) (map (x^) [0..])

-- cross multiply two vectors
-- cross [1,2,7] [6,7] -> [6,7,12,14,42,49]
cross xs ys = [x*y | x <- xs, y <- ys]

-- cross multiply two vectors, but filter to only those not exceeding 10^n.
cross' xs ys n = filter (<=(10^n)) (cross xs ys)

-- Hamming numbers of type n not exceeding 10^e.
-- The empty list is the Hamming numbers found so far (for recursion)
hamming n e = hamming' (primesTo n) [] e

-- Number of Hamming numbers of type n not exceeding 10^e.
numHamming n e = length (hamming n e)

-- If there are no more primes, then we are done
hamming' []     rs _ = rs
-- if we have no intermediate results yet, then it is just a list of the first primes powers
hamming' (p:ps) [] n = hamming' ps (powers p n) n
-- otherwise cross multiply the hamming numbers so far with the next primes powers
hamming' (p:ps) rs n = hamming' ps (cross' rs (powers p n) n) n

test204 = numHamming 5 8
pe204 = numHamming 100 9


-- 205
-- Dice Game: What is the probability that Pete 9 4-sided dice beats Colin (6 6-sided dice)? Give your answer rounded to seven decimal places in the form 0.abcdefg
-- Answer: 5731441 (0.01 secs, 3127216 bytes)
-- Analysis:  Sum the probabities that Pete will get each possible number 4..36, times the probability that Colin will get a
--            lower role.  The probability of a role is the number of ways that role can occur divided by the possible roles
--            (nsides^ndice).  Counting the ways to make a number is the hard part. Thanks to http://wizardofodds.com/gambling/dice/
--            the numbers for n dice can be determined from the numbers for n-1 dice.
-- counts is the number of ways that you can role an x with n-dice with m-sides (where x is a one-based index)
counts 1     nsides = replicate nsides 1
counts ndice nsides = replicate (ndice-1) 0 ++ [sumPriorBelow i | i <- [ndice..(ndice*nsides)]] ++ (replicate nsides 0)
  where prior = counts (ndice-1) nsides
        sumPriorBelow i = sum $ take (toEnd i) $ drop (toStart i) prior
        toStart i = max 0 (i-nsides-1)
        toEnd   i = min nsides (i-1)
-- pete = the probability that pete will role an i, where i is the (base 1) index of the list
-- pete' = the probability that pete will role an i or less where i is the (base 1) index of the list
pete = map (%(4^9)) $ counts 9 4
pete' = scanl1 (+) pete
colin = map (%(6^6)) $ counts 6 6
colin' = scanl1 (+) colin
tie = sum $ zipWith (*) pete colin
peteWin = sum $ zipWith (*) pete (0:colin')
colinWin = sum $ zipWith (*) colin (0:pete')
test205 = (peteWin, tie, colinWin, peteWin+tie+colinWin)
pe205 = floor (10^7 * peteWin + 0.5)


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


-- 210
-- Obtuse Angled Triangles: Find N(1,000,000,000) where N(r) is the number of points B in S(r) where triangle OBC has an obtuse angle
--                          where O is the point (0,0) and C is the point (r/4,r/4)
--                          and S(r) is the set of points (x,y) with integer coordinates satisfying |x| + |y| ≤ r
-- Answer: 1598174770174689458 (150.86 secs, 129714794848 bytes)
-- Answer: 1598174770174689458 (real 0m3.551s when compiled with ghc -O) 
-- Analysis: The math is easy to verify by drawing the simple cases on grid paper.
-- totals are easiest to obtain by counting along the diagonal  S(r) is a square rotated 45 degrees
-- the tricky ones are those within the circle of radius N*sqrt(2) and center N/8,N/8.  this is broken
-- down into the square bounded by O and C, and a few extras, checked individually in 1/8th of the circle,
-- being careful to not double counting overlapping points
-- this formula could be expressed much more succinctly, but this expression is hopefully more clear
pe210 = numObtuse 1000000000
  where
    numObtuse    n = (numSr n) - (numNotObtuse n)
    numSr        n = (n+1)^2 + n^2
    numNotObtuse :: Integer -> Integer
    numNotObtuse n = (numLines n) + (numOnNegDiag n) - (inSquareOC n) - (inCircle n)
      where
        nq = n `div` 4
        no = n `div` 8
        numLines     n = n+1 - (1 + nq)
        numOnNegDiag n = n+1 + nq * (n+1 + n)
        inSquareOC   n = (nq + 1)^2 - 2 - (nq+1)
        r2             = (fromIntegral nq)^2/2
        a              = fromIntegral no
        m              = floor (a * (sqrt 2 - 1))
        inCircle     n = 4*m + 8*(foldl' (+) 0 [no - 1 - (circley dx nq) | dx <- [1..m]])
          where
            circley dx nq = floor (a - (sqrt (r2 - ((fromIntegral (nq + dx)) - a)^2)))
