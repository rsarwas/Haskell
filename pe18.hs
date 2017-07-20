-- Project Euler in Haskell problems 341..360

import ProjectEuler

import qualified Data.Set as Set -- for problem 346

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
