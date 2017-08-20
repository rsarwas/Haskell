-- Project Euler in Haskell problems 181..200

import ProjectEuler

-- 187
-- Semiprimes: How many composite integers, n < 10^8, have precisely two, not necessarily distinct, prime factors?
-- Answer: 17427258 in 0m1.190s unix time (time python3 pe187.py)
-- Analysis:
--  2 can pair with all the primes below 10^8 /2
--  3 can pair with all the primes from 3 to 10^8/3
--  continue in this manner with each prime up to sqrt(10^8)

c n = (length primes) : [length (takeWhile (<=(n `div` x)) $ dropWhile (<x) primes) | x <- primes']
  where primes = primesTo (n `div` 2) -- all primes where prime * 2 <= primes
        primes' = takeWhile (<= (isqrt n)) $ tail primes  -- primes from 3 to sqrt n
pe187 = sum $ c (10^8)
-- Unfortunately, my prime number generator is too slow.  primesTo(10^6) takes over
-- 7 seconds.  primesTo 10^8/2 will be much more the 50 times longer.
-- the primesieve C library can do this in less than a second.  I used python
-- to implement the same algorithm, see pe187.py.
