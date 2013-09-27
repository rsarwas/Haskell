-- Project Euler in Haskell problems 61..80

import ProjectEuler

-- 62
-- Cubic Permutations: Find the smallest cube for which exactly five permutations of its digits are cube.
-- Answer: 127035954683 (13.29 secs, 1381871616 bytes)
find :: Int -> Int -> [([Int],[Int])] -> [Int]
find m n found = 
  let sortedDigits = quicksort $ digits (n^3)
      matches = findMatch sortedDigits found
      matching = if (null matches) then [] else (snd (head matches))
  in if ((length matching) == (m-1))
     then (n:matching)
     else (find m (n+1) ((sortedDigits, n:matching):found))
  where findMatch needle haystack = take 1 (filter (\a -> (fst a) == needle) haystack)
pe62 = (head $ quicksort (find 5 346 []))^3


-- 63
-- Powerful digit counts: How many n-digit positive integers exist which are also an nth power?
-- Answer (wrong): 21 (0.01 secs, 3683256 bytes)
pe63 = last $ takeWhile check [1..] where check x = x == (length $ digits (9^x))
-- I made an error in interpreting this problem as find the largest n (which is 21)
-- for any n, 10^n will have n+1 digits, n must be a positive integer.
-- for n = 1; [1..9]^1 = [1..9] all are 1 digit numbers, therfore there are 9 solutions with n = 1
-- for n = 2: [1..9]^2 = [1,4,9,16,..81] therefore there are 6 solutions for n = 2
-- for n = 3: [4..9]^3 = [64,125..??] therefore there are 5 solutions for n = 3
-- for n = 4: [5..9]^3 = [725,1296..??] therefore there are 4 solutions for n = 4
-- for n = 5: [6..9]^3 = [7776,16807..??] therefore there are 3 solutions for n = 5
-- continuing thus, there are 3@6, 2@7..10, and 1@11, we already know 9^21 is the last 21 digit number
-- Therefore the real Answer is 9+6+5+4+(3*2)+(2*4)+(1*11) = 49

-- 65
-- Convergants of e:  Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
-- Answer: 272 (0.01 secs, 4167024 bytes)
-- Analysis, convertants of continued fractions are given by hn/kn, where hn = an*hn-1 + hn-2, and kn = an*kn-1 + kn-2
-- where hn-2 = 0, hn-1 = 1. kn-2 = 1, hn-1 = 0.  the continued fraction of e is [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
-- starting with a0 = 2, a1 = 1, ...  the 100th convergant is at a99, but it is the 100th item in the continued fraction
e = reverse $ foldl (\a e -> 1:(2*e):1:a) [2] [1..33]
pe65 = sum $ digits $ eNumerator 100 where
  eNumerator n = last $ foldl contFrac [0,1] (take n e)
  contFrac [h2,h1] a = [h1,(a*h1+h2)]


-- 67
-- See pe4_67.hs


-- 69
-- Totient Maximum:  Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum
-- Answer: 510510 (0.01 secs, 3604432 bytes)
-- Analysis: t(n) = n*(1-1/p1)(1-1/p2)...(1-1/pr) where p1..pr are the prime factors of n
--   n/t(n) = p1/(p1-1) * p2/(p2-1) * ... * pr/(pr-1)
-- 2, 2/1, 3 => 3/2, 5 => 5/4, as pn grows pn/(pn-1) approaches 1
-- because duplicates of prime factors will result in a larger number without increasing the ratio n/t(n).
-- If there is a gap, and a larger prime, then number will be smaller  i.e. 2 * 3/2 > 2 * 5/4
-- therefore: The largest n/t(n) is the number with prime factors 2,3,5,7,11....
-- find the list of primes whose multiple is less than 10^6 then calculate n/t(n)
pe69 = head $ last $ takeWhile (\[n,d] -> n < 1000000) $ scanl (\[n,d] p -> [n*p,d*(p-1)]) [1,1] (primesTo 500)


-- 71
-- Ordered Fractions: By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size,
--                    find the numerator of the fraction immediately to the left of 3/7
-- Answer: 428570  (0.00 secs, 1543552 bytes)
-- Analysis: let the denominator start at 1000000 and work smaller (the larger the denominator,
--   the smaller the increments, therefore the closer it can get.  Then let the numerators start at the
--   integer closest to but less than (truncate) 3/7 of the denominators.  let the numerator proceed downward
--   until it gets to the last successful ratio of the denominator (starts with 2/5 of 1000000).
--   a numerator denominator combination is a successful candidate requires that gcd n d == 1.
--   The search can stop when the starting d and n is below the highest successful pair.
-- In testing the following routine, I found that the first fraction it returned was also the closest possible.
-- Wrong.  But in looking at all the other possible solutions, I found that the were lots of closer solutions,
-- And they were increasing. upto 428570 / 999997
closeFraction :: Int -> (Int,Int) -> (Int,Int) -> (Int,Int)
closeFraction d (n1,d1) (n2,d2) = let n1' = (d*n1) `div` d1 + 1
                                      n2' = d*n2 `div` d2
                                  in head [(n,d) | n <- [n2',(n2'-1)..n1'], gcd d n == 1]
test :: Int -> Bool
test d = let n = d*3 `div` 7
         in gcd d n == 1 && ((fromIntegral n)/(fromIntegral d) > 0.428571)
--pe71 = fst $ closeFraction 1000000 (2,5) (3,7)
pe71' = take 10 $ map (\d -> ((d*3 `div` 7),d)) $ filter test [999999,999998..7]
pe71'' = maximum (zip (map (\(n,d) -> (fromIntegral n)/(fromIntegral d)) pe71') [1..])

 

-- 76
-- Counting Summations:
-- Answer:
-- After looking at the 
sumOfSums n
  | n < 2     = error "number must be 2 or greater to have parts to sum"
  | n == 2    = 1
  | n == 3    = 2
  | otherwise = 2 + (sum (map sumOfSums' [2..(n-2)]))
  where sumOfSums' 2 = 2
        sumOfSums' n = 2 + (sum (map sumOfSums' [2..(n-1)]))


-- 79
-- Passcode derivation: Given that the three characters are always asked for in order, analyse the file
-- so as to determine the shortest possible secret passcode of unknown length.
-- Answer: 73162890 (solved manually)
-- by scanning the list manually, we can gather the following relationship
-- 1,2,3,6,7,8,9 < 0
-- 3,7           < 1 < 0,2,6,8,9
-- 1,3,6,7       < 2 < 0,8,9
-- 7             < 3 < 0,1,2,6,8,9
-- 1,3,7         < 6 < 0,2,8,9
--                 7 < 0,1,2,3,6,8,9
-- 1,2,3,6,7     < 8 < 0,9
-- 1,2,3,6,7,8   < 9 < 0
-- Therefore, 7 must be the first number, and 0 must be the last
-- eliminating them from all the list, we are left with 3 being second, and 9 being penultimate.
-- iterate thus twice more to get the answer.

