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
-- Pentagon Numbers: Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and D = |Pk - Pj| is minimised; what is the value of D?
-- A pentagon number, Pn=n(3n -1)/2 | n <- [1..] = [1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...]
-- Since a pentagon number is always positive, we will require j<k, so the difference is always positive.
-- rewriting the equation for pn, we get: 3n*n -n -2pn == 0; using the quadratic equation n =  (1 + sqrt(1+24pn))/6
-- Therefore pn is a pentagonal number if n is a natural number.
-- Answer:
pentagonNumbers = [(j,k,a,d) | j <- [1..10000000], k <- [(j+1)..(j+100)],
                    let tj = j*(3*j-1) `div` 2
                        tk = k*(3*k-1) `div` 2
                        a  = tj + tk
                        d  = tk - tj
                        sa = isqrt (1 + 24*a)
                        sd = isqrt (1 + 24*d),
                    sa*sa == 1 + 24*a,
                    (1 + sa) `mod` 6 == 0,
                    sd*sd == 1 + 24*d,
                    (1 + sd) `mod` 6 == 0]
-- I've checked with just a or just d, and I get plenty of hits, that are verifiably correct.
-- Ive gotten empty sets with j <- [1..100], k <- j+[1..100000] and visa-versa, as well as 1000,1000 
-- Nothing with j <- [1..10000000], k <- j+[1..100]


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


-- 47
-- Distinct primes factors: Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
-- The first three consecutive numbers to have three distinct prime factors are:
--   644 = 2^2 * 7 * 23;   645 = 3 * 5 * 43;   646 = 2 * 17 * 19
-- Note All 9 prime factors do not need to be distinct
-- Answer: 134043 (71.16 secs, 26183203160 bytes)
distinctPrimeFactorCount = length . unique . primeFactors
numbersWith4DistinctPrimeFactors = [x | x <- [1..], distinctPrimeFactorCount x > 3]
findFirstOfFour (r:s:t:u:xs)
  | r+1 == s && s+1 == t && t+1 == u = r
  | otherwise                        = findFirstOfFour $ (s:t:u:xs)
pe47 = findFirstOfFour numbersWith4DistinctPrimeFactors 


-- 48
-- Self Powers: Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-- Answer: 9110846700 (0.04 secs, 5809448 bytes)
pe48 = (sum [x^x | x <- [1..999]]) `mod` 10^10


-- 49
-- Prime permutations: What 12-digit number do you form by concatenating the three terms in the sequence described below?
-- The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
--  (i) each of the three terms are prime, and,
--  (ii) each of the 4-digit numbers are permutations of one another.
-- There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
--  but there is one other 4-digit increasing sequence.
-- Answer:
perm :: (Eq t) => Int -> [t] -> [t]
perm _ [] = error "Empty list"
perm 0 items = items
perm i items  
    | i <  0 = error "The permutation index must be a positive number"
    | i >= m2 = error "There are not that many permutations for these items"
    | otherwise = d:(perm r (take q items ++ drop (q+1) items))
        where
            (q,r) = i `quotRem` m1
            m1 = product [1..(l-1)]
            m2 = m1*l
            l =  length items
            d = items !! q
primes = dropWhile (<1000) $ primesTo 9999
primePerms n = filter isPrime $ unique $ quicksort $ map digitsToInt [perm x $ digits n | x <- [0..23]]
primePermSets = [pp | p <- primes, let pp = primePerms p, length pp >= 3]
nChoose3 :: [a] -> [(a,a,a)]
nChoose3 x 
  | length x < 3 = error "Must have 3 or more elements in a list to choose 3"
  | otherwise    = findSets x
  where findSets [a,b,c] = [(a,b,c)]
        findSets (x1:x2:xs) = [(x1,x2,xn) | xn <- xs] ++ findSets (x2:xs)


-- 50
-- Consecutive prime sum: Which prime, below one-million, can be written as the sum of the most consecutive primes?
-- i.e. The prime 41, can be written as the sum of six consecutive primes: 2 + 3 + 5 + 7 + 11 + 13
-- The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
-- Answer: (543,997651)  (2.88 secs, 1161946616 bytes)
-- considering only the first 1000 primes, there is a list 163 long, with a max at 76099.
-- if the list was only 163, then the largest prime could be 10^6/163 = 6135
--  (since primes are at most every other number, we could divide by 2, but since the numbers are increasing, there average value is 1/2)
-- considering only the first 3000 primes, there is a list 427 with a sum of 590819.
-- As more primes are considered, the average value increases, making the lists shorter; this was confirmed by looking at more primes
consecutiveSum :: Num a => [a] -> [a]
consecutiveSum = scanl1 (+)
partialLists :: [a] -> [[a]]
partialLists = scanr (:) []
consecutivePrimeSums n =
  let primes = primesTo 6135
  in [dropWhile (not . isPrime) $ sumsInRange p | p <- partialLists primes]
     where sumsInRange = reverse . takeWhile (<n) . consecutiveSum
info n = map (\x -> (length x, head x)) (consecutivePrimeSums n)
pe50 = maximum $ map (\x -> (length x, head x)) (consecutivePrimeSums 1000000)


-- 51
-- Prime digit replacements: Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.
-- By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values:
--   13, 23, 43, 53, 73, and 83, are all prime.
-- By replacing the 3rd and 4th digits of 56**3 with the same digit, this 5-digit number is the first example having
--   seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
--   Consequently 56003, being the first member of this family, is the smallest prime with this property.
-- Answer:


-- 52
-- Permuted multiples: 
-- Answer: 142857 (1.49 laptop secs, 181350776 bytes)
-- In order for x and 6x to have the same number of digits, x must begin with 1
pe52domain = [136..166] ++ [1136..1666] ++ [11136..16666] ++ [111136..166666]
pe52test x = and [all ((n `elem`) . digits) [2*x, 3*x, 4*x, 5*x, 6*x]  | n <- digits x] 
pe52 = head $ filter pe52test pe52domain


-- 53
-- Combinatoric selections: How many, not necessarily distinct, values of  nCr, for 1 <= n <= 100, are greater than one-million?
-- where n choose r = nCr = n! / r!(n-r)! where r <= n;
-- Answer: 4075 (0.08 secs, 75544368 bytes)
-- it is not until n = 23 that a value 23C10 = 1144066, exceeds one-million.
-- when n = r, then nChooseR = 1, so we can always ignore that
-- 5 choose r for r <- [1..5];  map (nChooseR 5) [1..4] = [5,10,10,5];
-- 6 choose r for r <- [1..6];  map (nChooseR 6) [1..5] = [6,15,20,15,6]
-- n choose r for r <- [1..n];  map (nChooseR 100) [1..99] = [100,4950,161700,3921225, ... ,3921225,161700,4950,100]
-- Therefore I can always ignore the first and last three results for 8 <= n; map (nChooseR n) [4..(n-4)]
-- if n is even, there are an odd number of results and largest value is at n `div` 2; results are symmetric about that index;
-- if n is odd, there are an even number of results and the largest value is at n `div` 2 and 1 + n `div` 2; results are symmetric about these indexes
-- I could optimize this by searching only half the r values, and stop at the first value that exceeds 1 000 000 (knowing the rest must exceed 1 000 000)
-- however, this simple solution is still wicked fast
pe53 = length $ filter (1000000<) $ concat [ map (nChooseR n) [4..(n-4)] | n <- [23..100]]


-- 54
-- 
-- Answer:


-- 56
-- Powerful digit sum: Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?
-- Answer: 972 (0.38 secs, 437951160 bytes); to search the entire space [1..99] for both a and b took only 3.19 seconds
-- Assume the maximum will be a number with a lot of digits (large exponent).  The answer 972/9 = 108, so the all possible
-- solutions will have at least 108 digits, therefore 54 is the smallest exponent to check. to verify no better solution exists
pe56 = maximum $ map (sum . digits) [a^b | a <- [1..99], b <- [1..99]]
