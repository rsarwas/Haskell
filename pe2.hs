import ProjectEuler

-- 26
--
-- Answer: 

-- 27 (algorithm needs optimzation)
-- Quadratic primes
-- Answer: -59231 (71 consecutive primes with a = -61 and b = 971)
--         (156.75 laptop secs, 5212541820 bytes)
pe27 = 
 let consecutivePrimes a b = length $ takeWhile (\n -> isPrime (n*n + a*n + b)) [0..]
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


-- 48
-- Self Powers: Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-- Answer: 9110846700 (0.04 secs, 5809448 bytes)
(sum [x^x | x <- [1..999]]) `mod` 10^10
