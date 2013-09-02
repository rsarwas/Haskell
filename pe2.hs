import ProjectEuler

-- 26
-- Reciprocal cycles: Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
-- Answer: 


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


-- 48
-- Self Powers: Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-- Answer: 9110846700 (0.04 secs, 5809448 bytes)
pe48 = (sum [x^x | x <- [1..999]]) `mod` 10^10
