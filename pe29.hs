-- Project Euler in Haskell problems 561..580

import ProjectEuler

-- 577
-- Counting hexagons: Find H(3) + H(4) + ... + H(12345) where H(3) is the number regular
--   hexagons on the lattice points in an equilateral triangle of integer side length n.
-- Answer:
-- Analysis:  First count the hexagons with sides parallel to the triangle.  There is
--   one hexagon of side length 1 (aka h1) in the triangle of side length 3 (aka t3).
--   There are 1 + 2 h1 in t4, and 1+2+3 in t5.  i.e. h1 = (n+2 -3)(n+1 -3)/n in tn.
--   At t6, we can fit 1 h2, at t7 we can fit 1+2, etc  h2 = (n+2 -6)(n+1 -6)/n in tn
--   None parallel hexagons

parallelHexagon :: Int -> [Int]
parallelHexagon n = [(n+2-t)*(n+1-t) `div` 2 | t <- [3,6..n]]
totalParallelHexagons :: Int -> Int
totalParallelHexagons n = sum [sum (parallelHexagon i) | i <- [3..n]]
-- totalParallelHexagons 12345 => 322628122094565 takes over 30 seconds to run, so we need to improve
sumParallelHexagons :: Int -> Int
sumParallelHexagons n = parallelHexagons n 1
-- n is the length of the bottom row,
-- m is the multiplier. we multiply the bottom row
-- by 1 and the row above by m+1; therefore we calculates the sum
-- of h(3) + h(4) + ... h(n)
parallelHexagons :: Int -> Int -> Int
parallelHexagons n m
  | n < 3 = 0
  | otherwise = m * sum (bottomHexagons n) + parallelHexagons (n-1) (m+1)
    where
      bottomHexagons n = takeWhile (>0) [n+1 - 3*i | i <- [1..]]
    -- list of hexagons of size 1, 2, 3, ... in the bottom of a triangle of size n
-- sumParallelHexagons 12345 => 322628122094565 also This also takes 30 seconds to run
