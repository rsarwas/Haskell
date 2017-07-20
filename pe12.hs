-- Project Euler in Haskell problems 221..240

import ProjectEuler

-- 233
-- Lattice Points: What is the sum of all positive integers N <= 10^11 such that f(N) = 420 ?
--   where f(N) is the number of points with integer coordinates that are on a circle passing through (0,0), (N,0),(0,N), and (N,N).
-- Answer:
-- Analysis:
sqrt2 = sqrt 2
isSquare x = x'*x' == x where x' = isqrt x
study n =
  let n2 = n `div` 2
      n22 = n2^2
      ul = truncate ((fromIntegral n2)*(sqrt2 -1))
  in [d | d <- [1..ul], isSquare (n22 - n*d - d*d)]
study' ns = filter (\(a,b) -> b > 0) (zip ns (map (length.study) ns))
curMax _     []             = []
curMax m (x:xs) | snd x >= m     = x:(curMax (snd x) xs)
                | otherwise = curMax m xs

-- study current max to 100,000 by 2 to 50, then by 10s (this took several minutes, getting progressively slower)
-- the first number is N, the second number is (F(N) - 4)/8, looking for 52
-- (10,1),(20,1),(26,1),(30,1),(34,1),(40,1),
-- (50,2),(100,2),
-- (130,4),(170,4),(260,4),(290,4),(340,4),(370,4),(390,4),(410,4),(510,4),(520,4),(530,4),(580,4),(610,4),
-- (650,7),(850,7),(1300,7),(1450,7),(1690,7),(1700,7),(1850,7),(1950,7),(2050,7),
-- (2210,13),(3770,13),(4420,13),(4810,13),(4930,13),(5330,13),(6290,13),(6630,13),(6890,13),(6970,13),(7540,13),(7930,13),
--           (8840,13),(9010,13),(9490,13),(9620,13),(9860,13),(10370,13),(10660,13),(10730,13),
-- (11050,22),(18850,22),(22100,22),(24050,22),(24650,22),(26650,22),(28730,22),
              (31450,22),(33150,22),(34450,22),(34850,22),(37570,22),(37700,22),(39650,22),
              (44200,22),(45050,22),(47450,22),(48100,22),(49010,22),(49300,22),(51850,22),(53300,22),(53650,22),
-- (55250,31),
-- (64090,40),(81770,40),(90610,40)
