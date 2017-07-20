-- Project Euler in Haskell problems 241..260

import ProjectEuler
import Data.Ratio -- for probem 243


-- 243
-- Resilience: Find the smallest denominator d, having a resilience R(d) < 15499/94744
-- Answer: 892371480 (0.01 secs, 2109192 bytes)
-- Analysis:  resiliancy, R(d) is equal T(d)/(d-1) where T(d) is the Euler totient or φ function,
--  See problem 69, and http://en.wikipedia.org/wiki/Euler's_totient_function
t :: (Integral a) => a -> a
t n = n * product [(pf-1) | pf <- pfs] `div` (product pfs)
      where pfs = unique $ quicksort $ primeFactors n
r :: (Integral a) => a -> Ratio a
r n = (t n) % (n-1)

--  by studying which values of d (upto 20000) create a new minimum, I noticed a pattern:
--  A minimum occurs where d is a product of primes, i.e d = 2*3, 2*3*5, 2*3*5*7,....
--  furthermore, there are additional minimums between these values at integer multiples of the last prime,
--  for example, minimums occur at 2*3*5*n where n = 1..7
study = curMin 1 [(d, r d) | d <- [2..20000]]
curMin _     []             = []
curMin m (x:xs) | snd x < m     = x:(curMin (snd x) xs)
                | otherwise = curMin m xs

-- finds solution at product [2,3,5,7,11,13,17,19,23] * 4
pe243 = fst $ head $ filter byCriteria [(x, r x) | n <- [2..lastPrime], let x = n*(product (primesTo (lastPrime-1)))]
  where
    byCriteria (n,x) = x < 15499 % 94744
    lastPrime = fst $ head $ filter byCriteria [(p, r x) | p <- (drop 5 (primesTo 500)), let x = (product (primesTo p))]
