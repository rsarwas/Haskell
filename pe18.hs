-- Project Euler in Haskell problems 341..360

import ProjectEuler

import qualified Data.Set as Set -- for problem 346

-- 345
-- See pe18_345.hs

-- 346
-- Strong Repunits: Find the sum of all strong repunits below 10^12
-- Answer: 336108797689259276 (11.43 secs, 6,465,907,200 bytes)
-- Analysis: repunits for base 2: binary:1 = decimal:1 = 2^0, b:11 = d:3 = 2^1 + 2^0,
-- b:111 = d:7 = 2^2+2^1+2^0, ... for any base, the nth repunit is base^(n-1) + ... base^0
-- this is easily expressed as a scan function (all_repunits base)
-- the first repunit is always 1, so we will ignore it.
-- the second is always b+1.  every number, n, will have a repunit 11 when the base is n-1,
-- so this will be the first in the pair.  we will ignore this repunit, and know that all
-- other repunits found have a match.
-- Since the third repunit is base^2 -1, we only need to look at base = sqrt(n), where n
-- We use a set to ignore duplicates

-- repunits returns all the decimal numbers that have a repunit in `base`, and are
-- less than 10^`power`
repunits :: Int -> Int -> [Int]
repunits base power = takeWhile (<(10^power)) $ drop 2 $ all_repunits base
  where all_repunits base = scanl1 (+) [base^x | x <-[0..]]

-- this takes about 7.5 seconds for power of 12 (0 seconds for power = 8)
strong_repunits :: Int -> [[Int]]
strong_repunits power = [repunits base power | base <- [2..(10^halfpower)]]
  where halfpower = if even power then power `div` 2 else power `div` 2 + 1

-- Using List operations this was way to slow. Still about 35% of execution time
unique_strong_repunits :: Int -> [Int]
unique_strong_repunits power = Set.toList $ foldl (\a x -> a `Set.union` (Set.fromList x)) Set.empty (strong_repunits power)

repunitsum :: Int -> Int
repunitsum power = 1 + (sum (unique_strong_repunits power))

test346 = repunitsum 3
pe346 = repunitsum 12


-- 357
-- Prime generating integers:
-- Find the sum of all positive integers n not exceeding 100 000 000 such that for every divisor d of n, d+n/d is prime.
-- Answer:
-- Analysis
--   1) Try Brute Force:
--     I only need to look at the first half of the divisors, due to symmetry:
--       for divisors of m {1, d2 ... dn-1, m}; 1 + m/1 == m + m/m; similarly for d2 and dn-1 etc.
--     If there is an odd number of divisors, then the middle divisor is a square root and this number can be ignored
--       because n/s = s and s + n/s = 2s which is not prime
--     I can ignore all odd numbers (except 1) 1 + n/1 is even and therefore not prime
--     Similarly I can ignore all multiples of 4, 9, 16, ....
--     Unfortunately, initial testing revealed that this approach is not nearly fast enough
--       10 000 < .5sec, while 100 000 > 10sec at best this implies 100 000 000 > 80000sec
--       even compiled, the code takes 22s for 10^6

halve' xs = take n xs where n = length xs `div` 2
halve xs = take n xs where n = (1 + length xs) `div` 2
divisor357s n = [d + n `div` d | d <- halve (divisors n)]
hasOnlyPrime357Divisors n = all isPrime (divisor357s n)
primeDivisorsBelow n = [x | x <- [1..n], even x, x `mod` 4 /= 0, x `mod` 9 /= 0, hasOnlyPrime357Divisors x]
pe357 = sum $ primeDivisorsBelow 1000000

--main :: IO ()
--main = print pe357
