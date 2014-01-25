-- Project Euler in Haskell problems 181..200

import ProjectEuler

-- 187
-- Semiprimes: How many composite integers, n < 108, have precisely two, not necessarily distinct, prime factors?
-- Answer:
-- Analysis:
--c n = (length primes) + (sum [length (takeWhile (<(n `div` x)) $ dropWhile (<x) primes) | x <- primes'])
c n = (length primes) : [length (takeWhile (<(n `div` x)) $ dropWhile (<x) primes) | x <- primes']
  where primes = primesTo (n `div` 2) -- all primes where prime * 2 <= primes
        primes' = takeWhile (<= (isqrt n)) $ tail primes  -- primes from 3 to sqrt n
