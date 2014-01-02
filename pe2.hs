-- Project Euler in Haskell problems 21..40

import ProjectEuler
import qualified Data.Set (fromList, member) -- for problem 23
import Data.Ratio -- for probem 33
import Data.List -- for sort and group in problem 39

-- 21
-- Evaluate the sum of all the amicable numbers under 10000
-- Answer: 31626  (11.05 laptop secs, 1074372488 bytes)
-- I thought it would be faster using Data.Map instead of an association list, but the time was the same

properDivisors = init . divisors
amicableNumbersTo n = fst $ foldl finder ([],[]) [1..n] where
  finder acc x
    | dx < x && (dx,x) `elem` potentialAmmicables =
               (dx:x:ammicables,potentialAmmicables)
    | x < dx = (ammicables, (x,dx):potentialAmmicables)
    | otherwise = acc
    where dx = sum $ properDivisors x
          ammicables = fst acc
          potentialAmmicables = snd acc
pe21 = sum $ amicableNumbersTo 9999


-- 22
-- See pe2_22.hs


-- 23
-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
-- Answer: 4179871 (29.37 secs, 14138726256 bytes)
-- A number is abundant if the sum of its proper divisors is greater than the number
-- We are given that all integers greater than 28123 can be written as the sum of two abundant numbers.
-- 12 is the first abundant number, and 24 is the first number that can be written as a sum of two abundant numbers
-- 28123 could be written as 12 + 28111 (if 28111 is abundant), providing the limits of the abundant numbers to explore
-- There are 6961 abundant numbers in this range.
-- All the time is spent generating the list of abundant numbers, so if additional improvement in time is needed.
-- either improve the speed of the properDivisors function, or find a faster way to determine if x is an abundant number
abundant n = n < (sum $ properDivisors n)
abundantNumbers = [x | x <- [12..28111], abundant x]
abundantSet = Data.Set.fromList abundantNumbers
notFromTwoAbundantSums x = null [a | a <- (takeWhile (<=(x `div` 2)) abundantNumbers), (x - a) `Data.Set.member` abundantSet]
pe23 = sum ([1..23] ++ (filter notFromTwoAbundantSums [25..28123]))


-- 24
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
-- Answer: 2783915460 (0.00 secs, 523500 bytes)
pe24 = digitsToInt $ lexiPerm (10^6-1) [0..9]


-- 25
-- What is the first term in the Fibonacci sequence to contain 1000 digits?
-- Answer: 4782 (0.03 secs, 14315648 byte)
fibWithDigits :: Int -> Int
fibWithDigits n = 1 + (length (takeWhile lessDigitsThan fibs))
  where lessDigitsThan = (>) (10^(n-1))
        fibs = 1 : scanl (+) 1 fibs 
pe25 = fibWithDigits 1000


-- 26
-- Reciprocal cycles: Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
-- Answer: d = 983 (982 digit cycle) (0.11 laptop secs, 13605860 bytes)
-- I only need to check primes, because the divisor can always be reduced to a set of prime factors
-- I am assuming a single prime factor will win, however I should check the multiple of two two digit primes
-- I am basically doing long hand division, I start with 1/p which has a remainder of 1, when I get a remainder of 1 again,
-- the cycle repeats  (if I get a remainder of 0, the cycle ends)
pe26 = maximum [(length $ div' 10 p,p) | p <- (primesTo 999)]
       where div' n x = let (q,r) = n `quotRem` x in if r < 2 then [q] else q:(div' (10*r) x)


-- 27
-- Quadratic primes
-- Answer: -59231 (71 consecutive primes with a = -61 and b = 971)
--         (3.02 new laptop secs, 5328626000 bytes)
--  speedup due to improvements in primeFactors (called by isPrime function)
pe27 = let consecutivePrimes a b = length $ takeWhile (\n -> isPrime (n*n + a*n + b)) [0..]
       in maximum [((consecutivePrimes a b),a,b) | a <- [(-999)..999], b <- primesTo 999]


-- 28
-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed with the number 1 and moving to the right in a clockwise direction
-- Answer: 669171001 (0.01 laptop secs, 1053000 bytes)
pe28 = sum [ring n | n <- [0..500]] where
       ring 0 = 1
       ring n = sum $ take 4 [largestCorner,largestCorner-delta..] where
                largestCorner = (2*n+1)^2
                delta = 2*n


-- 29
-- Distinct powers: How many distinct terms are in the sequence generated by a^b for 2 <= a <= 100 and 2 <= b <= 100?
-- Answer: 9183 (4.14 secs, 292505556 bytes)
pe29 = length $ unique $ quicksort $ [a^b | b <- [2..100], a <- [2..100]]


-- 30
-- Digit fifth powers:  Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
-- Answer: 443839 (4.46 laptop secs, 721162300 bytes)
-- upper limit is about 200,000 because 5*9^5 = 295245, 6*9^5 = 354294 so the largest possible sum less than itself will be at 199999
equalSumOfPowers x = x == sum (map (^5) (digits x))
pe30 = sum [x | x <- [10..200000], equalSumOfPowers x]


-- 31
-- Coin Sums:  How many different way can 2 pounds be made using any number of the 8 available coins
-- Answer: 73682 (0.07 laptop secs, 10532560 bytes)
--pe31 = [(l2,l1,p50,p20,p10,p5,p2,(200-l2-l1-p50-p20-p10-p5-p2)) | 
pe31 = length [ 1 | 
         l2 <- [0,200],
         l1 <- [0,100..(200-l2)],
         p50 <- [0,50..(200-l2-l1)],
         p20 <- [0,20..(200-l2-l1-p50)],
         p10 <- [0,10..(200-l2-l1-p50-p20)],
         p5 <- [0,5..(200-l2-l1-p50-p20-p10)],
         p2 <- [0,2..(200-l2-l1-p50-p20-p10-p5)]]


-- 32
-- Pandigital products : Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital
-- Answer: 45228 (1.03 laptop secs, 102910276 bytes)
-- only possible identies with 9 digits are {1..9}x{1000..9999} = {1000..9999} and {10..99}x{100..999}={1000..9999}
-- provide upper and lower limits which contain no duplicates or zeros
-- skip 1 and 9 on single digit, because 1 is identity (results in duplicates), and 9 times 1234 > 9999
isPandigital3 a b c = let digitsOfX = digits a ++ digits b ++ digits c
                      in (not ( 0 `elem` digitsOfX))
				         && (length digitsOfX) == (length $ unique $ quicksort digitsOfX)
identities = [(m1,m2,p) | m1 <- [12..98], m2 <- [123..(9999 `div` m1)], p <-[m1*m2], isPandigital3 m1 m2 p] ++
             [(m1,m2,p) | m1 <- [2..8], m2 <- [1234..(9999 `div` m1)], p <-[m1*m2], isPandigital3 m1 m2 p]
pe32 = sum $ unique $ quicksort $ map (\(_,_,x) -> x) identities


-- 33
-- Digit Canceling Fractions: If the lowest common denominator of the product of the four fractions where incorrect cancelling yields the correct result.
-- Answer: 100 (0.00 laptop secs, 0 bytes)
fracts = [ (n, d) | d <- [11..99], n <- [10..(d-1)],
                    let (n1,n2) = n `quotRem` 10
                        (d1,d2) = d `quotRem` 10
                    in (n1 == d2 && (n2 * d) == (d1 * n)) || (n2 == d1 && (n1 * d) == (d2 * n))]
-- using (%) and denominator from Data.Ratio to create a rational number, and get the denominator of a rational in lowest common form
pe33 = denominator $ product $ (map makeRat fracts)
       where makeRat x = (fst x) % (snd x)


-- 34
-- Digit factorials: Find the sum of all numbers (>2) which are equal to the sum of the factorial of their digits.
-- i.e. 1! + 4! + 5! = 1 + 24 + 120 = 145.
-- Answer: 40730 (21.49 secs, 6935205616 bytes)
-- [0!..9!] = [1,1,2,6,24,120,720,5040,40320,362880]   (scanl (*) 1 [1..9]
-- for a two digit number:
--   * no combination of {0!..3!} adds up to 10 (first two digit number), therefore any solution must have a 4 or greater.
--   * Since 5! > 99, any digit greater than 5 is not allowed.
--   * therefore, a two digit solution is a permutation of 4 and {0..4} => [14, 24, 34, 40, 41, 42, 43, 44]
-- Similar analysis can generate the list of 3 digit numbers [125, 135, 145, 150, 151, 152, 153, 154]
-- We can show that a 4 digit number must be greater than 1466
-- and that there is no way to create a 7, 8 or more digit number.
-- a huge optimization would be to limit the number of 5 and 6 digit number to check, eliminating most numbers below 1466 is insignificant.
digits2 = [14, 24, 34, 40, 41, 42, 43, 44]
digits3 = [125, 135, 145, 150, 151, 152, 153, 154]
pe34 = sum $ filter magic (digits2 ++ digits3 ++ [1466..999999])
    where magic x = x == (sum $ [factorials!!d | d <- digits x])
          factorials = scanl (*) 1 [1..9]
          
          
-- 35
-- Circular primes: How many circular primes are there below one million?
-- Answer: 55 (5.91 laptop secs, 754384904 bytes)
-- If any multi-digit prime has a 0,2,4,5,6, or 8, in it, then that digit will rotate to the last digit,
-- and ensure that it fails. Therefore, we only have to check primes with only 1,3,7,9
-- Skip checking all the 1 and two digit solutions, as they are provided in the problem statement
pe35 = 13 + (length $ filter isCircularPrime candidates)
       where candidates = concat (drop 2 (scanl f [1,3,7,9] [1..5]))
                          where f acc n = [a*10^n + b | a <- [1,3,7,9], b <- acc]
             isCircularPrime p = (isPrime p) && (and (map isPrime (map rotate [1..rots])))
                                 where rots = (length $ digits p) - 1
                                       rotate a =
                                         let (q,r) = p `quotRem` (10^a)
                                         in r*10^(rots+1-a) + q


-- 36
-- Double-base palindromes: Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2
-- Answer: 872187 (0.07 laptop secs, 18396560 bytes)
palindromes = [read (show x ++ (reverse $ show x)) :: Int | x <- [999,998..1]] ++ 
              [read (show x ++ show y ++ (reverse $ show x)) :: Int | x <- [99,98..1], y <- [9,8..0]] ++
              [10,9..0]
-- since the binary number must start with 1 (not 0), it must end with 1, so all the base 10 numbers are odd
pe36 = sum $ filter base2palindrome (filter odd palindromes)
       where base2palindrome x = let b = decToBin x in reverse b == b
             decToBin 0 = []
             decToBin y = let (a,b) = quotRem y 2 in decToBin a ++ [b]


-- 37
-- Truncatable primes: Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
-- Answer: 748317 (18.59 laptop secs, 2339299960 bytes)
-- Failed on first try, did not addjust list of candidates from problem 35
pe37 = sum $ take 11 (filter isTruncateablePrime (filter isPrime candidates))
       where candidates = concat (drop 1 (scanl f [1,2,3,5,7,9] [1..5]))
                          where f acc n = [a*10^n + b | a <- [1,2,3,5,7,9], b <- acc]
             isTruncateablePrime p = and (map isPrime (map truncateLeft [1..truncs] ++ (map truncateRight [1..truncs])))
                                 where truncs = (length $ digits p) - 1
                                       truncateLeft a = p `quot` (10^a)
                                       truncateRight a = p `rem` (10^a)


-- 38
-- Pandigital multiples: What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n  1?
-- Answer: 932718654 (0.04 secs, 5308316 bytes)
-- Since we were already given 9 * [1..5] = 918273645, this is a lower bounds for a solution
-- A better two digit integer must be in the range [92..98], but this would result in a 8 (2+3+3) or 12 (2+3+3+3) digit answer. NG
-- Similarly, a three digit integer [921..987] would result in a 7 or 11 digit solution. NG
-- However, a 4 digit integer [9183..9876] would result in 4+5 digit solution
-- A 5 or more digit integer is precluded since we need to multiply by 1..n where n>1, so [1,2] is minimal
-- At this point we can brute force a solution.
pe38 = head [(f a) | a <- [9876,9875..9183], isPandigital a, isPandigital (a*2), isPandigital (f a) ]
       where f x = x*10^5+x*2 
             isPandigital x = let digitsOfX = digits x
                              in (not ( 0 `elem` digitsOfX))
                                 && (length digitsOfX) == (length $ unique $ quicksort digitsOfX)


-- 39
-- Integer Right Triangles: For which perimeter p <= 1000 is there a maximum number of integer right triangles
-- Answer: 840 (0.03 secs, 7281032 bytes)
-- Analysis:  based on work done for problem 75 (see it for details)
--            could be made faster by creating a map from perimeter -> count when generating allPerims
basePerims = [2*m*(n+m) | n <- [1..nmax], m <- [(n+1)..(mmax n)], odd (m-n), gcd m n == 1]
  where
    nmax = isqrt 2500 --(1000 / 4)
    mmax n = ((isqrt (2*1000 + n^2)) - n) `div` 2
allPerims = [p*k | p <- basePerims, k <- [1..(1000 `div` p)]]
pe39 = snd $ maximum $ map (\l -> (length l,head l)) $ group $ sort allPerims


-- 40
-- Champernowne's constant: If dn represents the nth digit of the fractional part, find the value of d1 * d10 * ... * d10^6
-- Answer: 210 (laptop 0.00 secs, 528920 bytes)
champernowne n = (digits ((10^placeValue) + numberOffset)) !! digitOffset
  where  (placeValue,start) = last $ takeWhile (\x -> (snd x) < n) (scanl step (0,1) [0..])
            where step acc ele = (ele + 1, (snd acc) + (ele + 1) * 9 * 10^ele)
         (numberOffset,digitOffset) = (n - start) `quotRem` (placeValue+1)
pe40 = product [champernowne (10^x) | x <- [1..6]]