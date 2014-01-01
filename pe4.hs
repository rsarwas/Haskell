-- Project Euler in Haskell problems 61..80

import ProjectEuler
import Data.Ratio  -- for 80
import Data.List (sort,group)
import qualified Data.Map as Map -- for 74

-- 61
-- Cyclical figurate numbers: Find the sum of the only ordered set of six cyclic 4-digit numbers
--   for which each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, 
--   is represented by a different number in the set.
-- Answer: 28684 (0.02 secs, 6264000 bytes)
-- Analysis:  I make list of the 4 digit figurate numbers,  then I look for cycles as follows
--   for each oct number (the shortest list), I compare the last two digits against the first two digits
--   in the other lists.  for each hit, I add the number to my potential solution, and then compare that
--   with the remaining lists.  The implemtation is recursive.  The simple case is when there is only
--   one list remaining - then there is a solution if a number in the list matches the last number, and
--   then wraps around to match the first number.
oct n = n*(3*n - 2)
hep n = n*(5*n - 3) `div` 2	
hex n = n*(2*n - 1)
pen n = n*(3*n - 1) `div` 2
squ n = n*n
tri n = n*(n+1) `div` 2
t2l (a,b) = [a,b]
makePairs = map t2l . filter (\(a,b) -> b > 9) . map (\x -> quotRem x 100)
oct9999 =     (2 + (isqrt (4 + 12*9999))) `div` 6
oct1000 = 1 + (2 + (isqrt (4 + 12*1000))) `div` 6
octs = makePairs (map oct [oct1000..oct9999])
hep9999 =     (3 + (isqrt (9 + 40*9999))) `div` 10
hep1000 = 1 + (3 + (isqrt (9 + 40*1000))) `div` 10
heps = makePairs (map hep [hep1000..hep9999])
hex9999 =     (1 + (isqrt (1 + 8*9999))) `div` 4
hex1000 = 1 + (1 + (isqrt (1 + 8*1000))) `div` 4
hexs = makePairs (map hex [hex1000..hex9999])
pen9999 =     (1 + (isqrt (1 + 24*9999))) `div` 6
pen1000 = 1 + (1 + (isqrt (1 + 24*1000))) `div` 6
pens = makePairs (map pen [pen1000..pen9999])
squ9999 =      isqrt 9999
squ1000 = 1 + (isqrt 1000)
squs = makePairs (map squ [squ1000..squ9999])
tri9999 =     ((isqrt (1 + 8*9999)) - 1) `div` 2
tri1000 = 1 + ((isqrt (1 + 8*1000)) - 1) `div` 2
tris = makePairs (map tri [tri1000..tri9999])

cyclical (x:xs) [a] = 
   let solution = [h:x:xs | [h,t] <- a, t == x, (last xs) == h]
   in if (null solution) then [] else (head solution)
cyclical (x:xs) as =
   let solution = filter (not . null) [ cyclical (h:x:xs) (filter (/=a) as) | a <- as, [h,t] <- a, t == x]
   in if (null solution) then [] else (head solution)

pe61 = sum $ makeNums $ numberParts where
   numberParts = head $ filter (not . null) [cyclical x [heps, hexs, pens, squs, tris] | x <- octs]
   makeNums [h,t] = [(h*100+t)]
   makeNums (h:t:xs) = (h*100+t):(makeNums (t:xs))


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
-- Answer (wrong, see notes below): 21 (0.01 secs, 3683256 bytes)
pe63 = last $ takeWhile check [1..] where check x = x == (length $ digits (9^x))
-- I made an error in interpreting this problem as find the largest n (which is 21)
-- for any n, 10^n will have n+1 digits, n must be a positive integer.
-- for n = 1; [1..9]^1 = [1..9] all are 1 digit numbers, therfore there are 9 solutions with n = 1
-- for n = 2: [1..9]^2 = [1,4,9,16,..81] therefore there are 6 solutions for n = 2
-- for n = 3: [4..9]^3 = [64,125..??] therefore there are 5 solutions for n = 3
-- for n = 4: [5..9]^4 = [625,1296..??] therefore there are 4 solutions for n = 4
-- for n = 5: [6..9]^5 = [7776,16807..??] therefore there are 3 solutions for n = 5
-- continuing thus, there are 3@6, 2@7..10, and 1@11, we already know 9^21 is the last 21 digit number
-- Therefore the real Answer is 9+6+5+4+(3*2)+(2*4)+(1*11) = 49


-- 64
-- Odd period square roots: How many continued fractions for sqrt(N) <= 10000 have an odd period?
--   where N is not square, and period is the number of terms in the repeating part of the CF.
-- Answer: 1322  (1.80 secs, 265677000 bytes)
-- for fun, I practiced by creating a continued fraction generator for normal fractions.
cf n 0 = []
cf 0 d = [0]
cf n 1 = [n]
cf n d  | n < d     = 0:(cf d n)
        | otherwise = q:(cf d r) where (q,r) = n `quotRem` d
-- From Expressing square roots as continued fraction at 
-- http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
cfsq n = sq' 0 1 ao ao n where ao = isqrt n
sq' m d ao a s = 
  let m' = d*a - m
      d' = (s - m'*m') `div` d
      a' = (ao + m') `div` d'
  in if a' == 2*ao then a:[a'] else a:(sq' m' d' ao a' s)
-- These expansions include the first term, so if we subtract 1
period n = [(length $ cfsq x) -1 | x <- [2..n], let xs = isqrt x, xs * xs /= x] 
pe64 = length $ filter odd (period (10^4))


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


-- 66
-- Diophantine equation: Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained
--                       in the diophantine equation of x^2 – Dy^2 = 1
-- Answer: 661  (0.14 laptop secs, 43985600 bytes)
-- Analysis: The problem can also be stated as finding the fundamental solution to Pell's equation,
--           which may be found by performing the continued fraction expansion and testing each successive
--           convergent until a solution to Pell's equation is found. See http://en.wikipedia.org/wiki/Pell's_equation.
--           See pe64 for the cf of the sqrt of n (modified to not stop at the end of the repeating part)
--           See pe65 for the convergents of the cf (modified to be a continuous list of numerator & denominator)
cfsq' n = sq'' 0 1 ao ao n where ao = isqrt n
sq'' m d ao a s = 
  let m' = d*a - m
      d' = (s - m'*m') `div` d
      a' = (ao + m') `div` d'
  in a:(sq'' m' d' ao a' s)
convergents xs = convergents' (0,1) (1,0) xs
  where convergents' (h2,k2) (h1,k1) (a:xs) = (h,k):(convergents' (h1,k1) (h,k) xs)
         where h = a*h1 + h2
               k = a*k1 + k2
fundamentalX n = head [(x,y) | (x,y) <- convergents (cfsq' n), x*x-n*y*y == 1] where m = isqrt(n)
pe66 = snd $ maximum $ [(fundamentalX d,d) | d <- [1..1000], not $ (d `elem` squares)]
  where squares = [x^2 | x <- [1..(isqrt 1000)]]   


-- 67
-- See pe4_67.hs


-- 68: Magic 5-gon ring
-- Answer: 6531031914842725 (1.52 laptop secs, 447672240 bytes)
-- Analysis:
--  if we assign indexes from 1 to n counting clockwise on the outside, then the inside, (starting the inside at the same
--   place as we started the outside), we can determine if a permutation of n*2 ints is a magic N-gon
--  for the 5-gon, the 10 must be in the outer loop, else it will be used twice, and we will get a 17 digit number
--  The largest number will start with n+1 with n+1..2n in the outer loop, unless there is no solution with that
--  configuration.  we should assume there is (like for the 3-gon)

--  permy n, return a list of all unique permutations of n*2 digits with lowest of first 3 at head
--  i.e 123456 == 312645 == 231564, and we should only consider 123456
--  also, only consider n+1..2n in the first n positions, since we are looking for the largest number
--  for n=3 [4,5,6]++all perms of [1,2,3] and [4,6,5] with all perms of [1,2,3]
--  for n=5 [6] ++ perms of 7..10 ++ perms 1..5
permy n = [ (n+1):(a ++ b) | a <- permy' [(n+2)..(2*n)], b <- permy' [1..n]]
permy' [x] = [[x]]
permy' (x:xs) = map (x:) ps ++ 
                [start ++ [x] ++ end | p <- ps, i <- [1..(length ps - 1)], let (start,end) = splitAt i p] ++
                map (++[x]) ps
                   where ps = permy' xs

ngon n xs = [ngonLeg i n xs | i <- [0..(n-1)]]
ngonLeg i n xs = (take 1 $ drop i xs) ++ (take 2 $ drop (n+i) xs) ++ if i == (n-1) then [xs!!n] else []
allEqual (x:xs) = and $ map (x==) xs
isMagic = allEqual . map sum
magicngons n = filter isMagic [ngon n xs | xs <- permy n]
--pe68 = digitsToInt $ concat $ maximum $ magicngons 3
pe68 = concat $ maximum $ magicngons 5   -- digitsToInt does not work with 2 digit ints (i.e. 10)


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


-- 70
--Totient permutation: Find the value of n, 1  n  10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.
-- Answer: 8319823 (0.42 secs, 52987764 bytes)
-- Analysis: the minimum ratio would be for the largest prime number since φ(n) = n-1 when n is prime.
--   However, n-1 and n can never satisfy the permutation requirement, so ignore all primes.
--   Otherwise, the minimum ratio is with two large prime factors. so look for primes close to sqrt (10^7)
--   φ(n) = n(1 - 1/p1)(1 - 1/p2) .. (1 - 1/pn)  if there are no duplicate prime factors then φ(n) = (p1-1)(p2-1)..(pn-1)
--   The smallest ratio I found was at 2609*2693.  at 215*215*215 (approx. three largest prime factors), the ratio is
--   much larger than the solution, s we do not need to check solutions with 3 prime factors.
-- This permutation check is simple but slow 
permutation n1 n2 =
  let n1s = quicksort $ show n1
      n2s = quicksort $ show n2
  in n1s == n2s
perms = [(n,t) | let p = reverse $ primesTo 3162, a <- p, b <- dropWhile (>a) p,
                 let n = a*b, let t = (a-1)*(b-1), permutation n t]
-- This is empty, but I needed to check, because 3119^2 had a smaller ratio than the other winners
perms' = [(n,t) | p <- reverse $ primesTo 3162, let n = p*p, let t = (p-1)*(p-1), permutation n t]
-- 7026037 = 2609*2693 is not right, but I forgot to check prime factors larger than 3162.
-- Since we know 10^7/2609 = 3832, we search in this range to find a solution with a better ratio.
perms'' = [(n,t) | let p = reverse $ dropWhile (<2609) $ primesTo 3832, a <- p, b <- dropWhile (>a) p,
                 let n = a*b, let t = (a-1)*(b-1), n < 10000000, permutation n t]
-- That didn't help.  Solution must have a larger range, however if pf = 10^4 and 10^3, then ratio = 1.00101,
-- but the ratio for 7026037 is 1.0007550490432913
perms''' = [(n,t) | let p = reverse $ dropWhile (<2000) $ primesTo 4000, a <- p, b <- dropWhile (>a) p,
                 let n = a*b, let t = (a-1)*(b-1), 7026037 < n, n < 10000000, permutation n t]
--That did it.  It found the solution at 2339*3557 with a ratio of 1.0007090511248113
pe70 = snd $ minimum $ map (\(n,t) -> ((fromIntegral n)/(fromIntegral t),n)) (perms''')


-- 71
-- Ordered Fractions: By listing the set of reduced proper fractions for d ≤ 1,000,000 in ascending order of size,
--                    find the numerator of the fraction immediately to the left of 3/7
-- Answer: 428570  (0.01 secs, 2615184 bytes)
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
nForMaxD = fst $ closeFraction 1000000 (2,5) (3,7)
test :: Int -> Bool
test d = let n = d*3 `div` 7
         in gcd d n == 1 && ((fromIntegral n)/(fromIntegral d) > 0.428571)
pe71 = fst $ snd $ maximum $ take 10 $ map (\(n,d) -> ((fromIntegral n)/(fromIntegral d),(n,d))) possible
   where possible = map (\d -> ((d*3 `div` 7),d)) $ filter test [999999,999998..7]


-- 72
-- Counting Fractions:  How many elements would be contained in the set of reduced proper fractions for d <= 1,000,000?
-- Answer: 303963552391 (929.59 secs, 19658268356 bytes)  15.5 minutes
-- Analysis: The number of fractions for each denominator is the Euler phi function,
t :: (Integral a) => a -> [a] -> a 
t 1 _ = 1
--t n = (fromIntegral n) * product [1 - 1/(fromIntegral p) | p <- (unique $ primeFactors n)]
t n ps = n * (product [(p - 1) | p <- upf]) `div` (product upf)
        where upf = unique $ primeFactors' n ps
sumTot :: (Integral a) => a -> a
sumTot n = sum [t d ps | d <- [1..n]] where ps = primesTo (isqrt n)

-- Using the Totient Summatory Function from Wolfram
-- Improved effiency, by short circuiting when I can, and using precomputed primes list
mu :: (Integral a) => a -> [a] -> a
mu x ps
   | x == 1              = 1
   | null ps || q' == 0 = -1
   | r == 0  && r' == 0  = 0
   | r == 0              = (-1)*(mu q (tail ps)) 
   | otherwise           = mu x (tail ps) 
   where nextPrime = head ps
         (q,r) = x `quotRem` nextPrime 
         (q',r') = q `quotRem` nextPrime 
sumTot2 :: (Integral a) => a -> a
sumTot2 n = sum [mu d ps * flr * (1+flr) | d <- [1..n], let flr = n `div` d] `div` 2
            where ps = primesTo (isqrt n)

-- Approximation from Wolfram 
--   This is very fast, but off by 1464 -- error term is O(10^7)
sumTot3 x = 3*x*x/pi/pi
sumTot3Error x = x * (log x)**(2/3) * (log (log x))**(4/3)

pe72 = sumTot2 (10^6) - 1


-- 73
-- Counting fractions in a range: How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?
-- Answer: 7295372 (22.21 secs, 11606982560 bytes)
-- Analysis: similar to 71, We figure the min and max numerator for a denominator, then find which of those are proper reduced fractions
count d (n1,d1) (n2,d2) =
  let n'  = 1 + n1*d `div` d1
      n'' =     n2*d `div` d2
  in sum [1 | n <- [n'..n''], gcd n d == 1]
pe73 = sum $ map (\x -> count x (1,3) (1,2)) [5..12000]


-- 74
-- Digit factorial chains
-- Answer: 402 (8.30 secs, 8353854992 bytes)
-- Analysis: mostly brute force, however, we can see that using lists will take way to long, so we use a hash table (Map)
--  saving all the intermediate results, so if those numbers are hit, we can short circuit the check.
-- optimization: recognizing that digitFactorial of 103 is the same as 130 as 301 as 310 (but not 013, or 031),
-- so we reverse sort the digits, and check/store that as well.
-- A better optimization would be to only check monotonically decreasing numbers, and then figure the number of permutations
-- for each number that hit 60.  I didn't bother with that, since this solution is good enough.

factdig 0 = 1
factdig 1 = 1
factdig 2 = 2
factdig 3 = 6
factdig 4 = 24
factdig 5 = 120
factdig 6 = 720
factdig 7 = 5040
factdig 8 = 40320
factdig 9 = 362880
digitFactorial = sum . map factdig . digits

sl 1 = 1
sl 2 = 1
sl 145 = 1
sl 871 = 2
sl 872 = 2
sl 45361 = 2
sl 45362 = 2
sl 40585 = 1 -- from pe34
sl 169 = 3
sl 1454 = 3
sl 363601 = 3
sl n = 1 + sl (digitFactorial n)
pe74' x = filter (\(n,l) -> l == 60) [(n,sl n) | n <- [1..x]]
--estimated time for simple solution is 800 seconds, based on .8 seconds for 10^3, 8 seconds for 10^4


maxPerm = digitsToInt . reverse . sort . digits

myInsert n m =
  case Map.lookup n m of
    Nothing  -> let n' = maxPerm n
  				 in case Map.lookup n' m of {
  				      Nothing  -> let (m',l) = myInsert (digitFactorial n) m
  				                      l'     = l + 1
  				                      m''    = Map.insert n' l' (Map.insert n l' m')
                                   in (m'',l');
                      (Just l) -> (Map.insert n l m,l) }
    (Just l) -> (m,l)

initialLengths = Map.fromList [(1,1), (2,1), (145,1), (169,3), (871,2), (872,2), (1454,3), (40585,1), (45361,2), (45362,2), (363601,3)] 
allLengths = fst $ foldr (\k acc -> myInsert k (fst acc)) (initialLengths,0) [1..999999]
pe74 =  length $ filter (\(n,l) -> l == 60 && n < 999999) $ Map.toList allLengths

-- 75
-- Integer Right Triangles: Given that L is the length of the wire, for how many values of L <= 1,500,000 can exactly
--                          one integer sided right angle triangle be formed?
-- Answer: 161667  (3.00 new laptop secs, 2205145384 bytes)
-- Analysis:  for a pythagoream triple, a = k(m^2 - n^2), b = k(2mn) and c = k(m^2+n^2), where m > n, and m-n is odd
--            if m and n are coprime, and k = 1, then it the solution is primative (the smallest in a family).
--            L = P = a+b+c = k(2m^2 + 2mn) = k2m(m+n)
--            since m > n, if we assume m = n+1, we can find the upper bound of n = 611 for P <= 1,500,000
--            m has a max for each value of n that is solution to a quadratic equation.
basePerims = [2*m*(n+m) | n <- [1..611], m <- [(n+1)..(mmax n)], odd (m-n), gcd m n == 1]
  where mmax n = ((isqrt (3000000 + n^2)) - n) `div` 2
allPerims = [p*k | p <- basePerims, k <- [1..(1500000 `div` p)]]
--takes about 0.4 sec to generate ~98,000 unique basePerims and about 2 more seconds to generate 1,250,000 allPerims with dups
--sorting with data.list takes about 5 seconds.  This is probably not the smartest/fastest way to do this, but it is simple and works.
--I'm using the data.list group, because it is way faster than the group I have in ProjectEuler, which does not require a sorted array.
--pe75 = length $ filter (\(_,n)-> n ==1) $ group $ sort allPerims
pe75 = length $ filter ((==1).length) $ group $ sort allPerims


-- 76
-- Counting Summations:
-- Answer: 190569291  (0.01 secs, 2230888 bytes) (recursive solution: 1009.07 secs, 237087938760 bytes)
-- After studying the pattern 1,2,4,6,10,14,20,29,41,... and trying several failed attempts, I stumbled upon the
-- following (from http://www.mathpages.com/home/kmath091.htm):
-- (1) The number of partitions into k or fewer parts is equal to the number of partitions into exactly k parts plus
--     the number of partitions into k-1 or fewer parts.
-- (2) Given a partition of n into exactly k parts, we can  subtract 1 from each part, leaving a partition of n-k
--     in k or fewer parts.  Thus there is a one-to-one correspondence between the partitions of n into exactly k
--     parts and the partitions of n-k into k or fewer parts.
-- Putting these two facts together, we have the simple recurrence relation A_k(n)  =  A_{k-1}(n)  +  A_k(n-k)
sumOfSums _ 0 = 1
sumOfSums _ 1 = 1
sumOfSums 1 _ = 1
sumOfSums k n = if n < 0 then 0 else (sumOfSums (k-1) n) + (sumOfSums k (n-k))
-- However sumOfSums 99 100 generated the correct solution but it took almost 17 minutes.
-- (note sumOfSums 100 100, includes a singleton 100 in the list, so it is 1 too many.)
-- Then I found the algorithmic solution:
-- Note that this solution includes the n, so the total for 5 is 7, because it includes the singleton of 5.
-- for this problem, we just subtract 1 from the total.
-- For more info, see: http://en.wikipedia.org/wiki/Partition_function_%28number_theory%29#Partition_function
-- p(n) = p(n-1) + p(n-2) - p(n-5) - p(n-7) + p(n-12) + p(n-15) - ...
-- or p(n) = ∑ k ∈ [1,n) q(k) p(n-k)
-- This Haskell code was stolen from http://stackoverflow.com/questions/18157582/haskell-list-generator
-- q( i ) | i == (3k^2+5k)/2 = (-1) ^ k
--        | i == (3k^2+7k+2)/2 = (-1) ^ k
--        | otherwise         = 0
-- q = [1,1,0,0,-1,0,-1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,-1,0,0,0,-1,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0..]
-- 1, skip 0, 1 ,skip 2, -1, skip 1, -1, skip 4, 1, skip 2, 1, skip 6, -1, skip 3, -1, skip 8, 1, skip 4,...
q = go id 1
  where go zs c = zs . zs . (c:) . zs . (c:) $ go ((0:) . zs) (negate c)
p = map head $ iterate next [1]
   where next xs = sum (zipWith (*) q xs) : xs
pe76 = p !! 100 - 1


-- 77
-- Prime summations: What is the first value which can be written as the sum of primes in over five thousand different ways?
-- Answer: 71  (0.43 secs, 415623888 bytes)
countBelow n n' = sum [f x y | x <- primesTo (min (n-2) n'), let y = n - x]
  where
    f x y
      | x == 2    = if odd y then 0 else 1
      | isPrime y = if y > x then (countBelow y x) else (1 + countBelow y x)
      | otherwise = countBelow y x
pe77 = fst $ head $ filter (\(x,y) -> y > 5000) [ (x,countBelow x x) | x <- [11..]]


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
pe79 = 73162890


-- 80
-- Square root digital expansion: For the first one hundred natural numbers, find the total of the digital sums of the
--                                first one hundred decimal digits for all the irrational square roots.
-- Answer: 40886 (0.68 laptop secs, 303187096 bytes)
-- by experimentation, we need to go to an epsilon of 1/10^102 to avoid rounding errors in the last digits
pe80 = sum [sumSqRootDigits 100 x | x <- [1..100], not $ x `elem` squares]
  where
    squares = [x^2 | x <- [1..10]]
    sumSqRootDigits d n = sum $ take d $ digits $ sqrtTo (d+2) n
    sqrtTo n x = 
      let a = sqrteps (1 % (10^n)) x
      in (numerator a)*(10^n) `div` (denominator a)

-- From http://hackage.haskell.org/package/numbers-3000.2.0.0/docs/src/Data-Number-FixedFunctions.html#fromCF
sqrteps :: Rational -> Rational -> Rational
sqrteps eps x = approxCF eps ((m,x-m^2):[(2*m,x-m^2) | r<-[0..]])
        where
            m = (isqrt  (floor x))%1
approx      :: Rational -> Rational -> Rational
approx eps x = approxRational x eps
approxCF :: Rational -> [(Rational, Rational)] -> Rational
approxCF eps [] = 0
approxCF eps x
        --
        -- Approximate infinite continued fraction x by fraction,
        -- evaluating from left to right, and stopping when
        -- accuracy eps is achieved, or when a partial numerator
        -- is zero -- as it indicates the end of CF.
        --
        -- This recursive function relates continued fraction
        -- to rational approximation.
        --
        = approxCF' eps x 0 1 1 q' p' 1
            where
                h = fst (x!!0)
                (q', p') = x!!0
                approxCF' eps x v2 v1 u2 u1 a' n
                    | abs (1 - f1/f) < eps = approx eps f
                    | a == 0    = approx eps f
                    | otherwise = approxCF' eps x v1 v u1 u a (n+1)
                    where
                        (b, a) = x!!n
                        u  = b*u1 + a'*u2
                        v  = b*v1 + a'*v2
                        f  = u/v
                        f1 = u1/v1
 
