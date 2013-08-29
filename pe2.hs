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
