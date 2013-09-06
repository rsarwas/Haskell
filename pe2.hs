-- Project Euler in Haskell problems 21..40

import ProjectEuler


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
-- Answer: 
abundant n = n < (sum $ properDivisors n)
abundantNumbers = [x | x <- [1..], abundant x]
pe23 = takeWhile (<10000) abundantNumbers


-- 24
-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
-- Answer: [2,7,8,3,9,1,5,4,6,0] (0.00 secs, 523500 bytes)
lexiPerm :: (Eq t) => Int -> [t] -> [t]
lexiPerm _ [] = error "Empty list"
lexiPerm 0 items = items
lexiPerm i items  
    | i <  0 = error "The permutation index must be a positive number"
    | i >= m2 = error "There are not that many permutations for these items"
    | otherwise = d:(lexiPerm r [x | x <- items, x /= d])
        where
            (q,r) = i `quotRem` m1
            m1 = product [1..(l-1)]
            m2 = m1*l
            l =  length items
            d = items !! q
pe24 = lexiPerm (10^6-1) [0..9]


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


-- 27 (algorithm needs optimzation)
-- Quadratic primes
-- Answer: -59231 (71 consecutive primes with a = -61 and b = 971)
--         (156.75 laptop secs, 5212541820 bytes)
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
-- Digit Canceling Fractions: 
-- Answer:


-- 34
-- 
-- Answer: 


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
-- Answer: 840 (111.71 laptop secs, 14777564224 bytes)
-- lots of permimeters have no integral right triangles, can we identify those and not check?
pe39 = snd $ maximum [(length $ rightTriangles p,p) | p <- [12..1000]]
       where rightTriangles p = 
               [(a,b,c) | a <- [1..(p `div` 3)], b <- [(p `div` 3)..(p `div` 2)], c <- [p - a - b], c^2 == a^2 + b^2]


-- 40
-- Champernowne's constant: If dn represents the nth digit of the fractional part, find the value of d1 * d10 * ... * d10^6
-- Answer: 210 (laptop 0.00 secs, 528920 bytes)
champernowne n = (digits ((10^placeValue) + numberOffset)) !! digitOffset
  where  (placeValue,start) = last $ takeWhile (\x -> (snd x) < n) (scanl step (0,1) [0..])
            where step acc ele = (ele + 1, (snd acc) + (ele + 1) * 9 * 10^ele)
         (numberOffset,digitOffset) = (n - start) `quotRem` (placeValue+1)
pe40 = product [champernowne (10^x) | x <- [1..6]]