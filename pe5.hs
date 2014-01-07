-- Project Euler in Haskell problems 81..100

import ProjectEuler
import Data.Set (fromList, size)  --for uniquing in 87

-- 81
-- See pe5_81.hs


-- 82
-- See pe5_82.hs


-- 83
-- See pe5_83.hs


-- 85
-- Counting rectangles: Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution.
-- Answer: 2772 (0.00 secs, 2059720 bytes)
-- Analysis: a grid that is 1xn units, has 1 rect 1xn, 2 rects 1x(n-1).  ... n rects (1x1)
--   therefore, there are 1+2+...+n rectangles, this is the nth triangle number t(n) = n(n+1)/2
--   If the grid is 1 unit high, there is 1 collection with t(n), if the grid is 2 units high,
--   there is 1 collection 2 units high, and 2 collections 1 unit high, or three total collections
--   at this point it should be clear that there are t(m) collections in a grid m units high.
--   therefore if the grid is n x m, there are t(n)*t(m) = n(n+1)m(m+1)/2/2 rectangles
--   testing this with the 2x3 grid in the problem statement, we get 18 rectangles as shown.
--   therefore, we are looking for m and n which minimize abs(2000000 - n(n+1)m(m+1)/2/2).
--   at one extreme, m = 1 (or n = 1), and n(n+1) = sqrt 4000000 = 2000, so n = 1990 or 2000,
--   1x1999 = 1999000 rectangles, and 1x2000 = 2001000 rectangle, both 1000 rectangles away.
--   assuming that there is an unambiguous best answer, the long skinny solution is not the answer.
--   at the other extreme, the grid is square (or nearly so) n ~ 4th root of 8000000 ~ 53.183
--   53x53 = 2047761, or 47761 over, much further from the goal.
--   if we have a width n, then the height m would be m(m+1) = 2 * 2 * 2000000 / n / (n+1)
--   therefore, the if low = sqrt (2 * 2 * 2000000 / n / (n+1), the two options are low-0.5 and low+0.5
--   if we look at the grids 1 to 53 high, we can calculate the two lengths on both sides of 2000000
rectCount n m = n*(n+1)*m*(m+1) `div` 4
widthOptions h = let h' = fromIntegral h in let low = truncate $ sqrt ((8000000 / h' / (h'+1)) - 0.5) in (low,low+1)
rects = foldl (\rects h -> let (w1,w2) = widthOptions h in [h,w1]:[h,w2]:rects) [] [1..53]
closest = minimum $ map (\[h,w] -> [(abs (2000000 - (rectCount w h))),h,w]) rects
pe85 = let [diff,h,w] = closest in w*h


-- 87
-- Prime power triples: How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
-- Answer: 1097343 (8.46 secs, 1412321004 bytes)
-- Analysis: Check all prime upto to the fourth root of 50e6 for the fourth power term.  With each of those,
--   check all primes up to the cube root of remainder, continue in this way to the square root.
--   The biggest speed up comes from precomputing a single list of primes that are filtered as necessary.
--   This will create some duplicates, which must found and removed;
--   This brute force solution takes about 1 seconds to create over one million numbers,
--   and then an additional 56 seconds to remove about 40,000 non-unique solutions.
--   There are much better uniquing solutions using hash tables.
--   There might be a trick to eliminate duplicates before adding to the list (i.e. does it help to know 4^2 = 2^4)
primePowerTriples n = [p | c <- takeWhile (<=(max4 n)) ps, let c4 = c^4, b <- takeWhile (<=(max3 (n-c4))) ps, let b3 = b^3, a <- takeWhile (<=(max2 (n-c4-b3))) ps, let p = a^2 + b3 + c4]  -- 1.3 sec (but 338 seconds to quicksort - out of order)
   where ps = primesTo (max2 n)
         max2 x = floor ((fromIntegral x)**(1/2))
         max3 x = floor ((fromIntegral x)**(1/3))
         max4 x = floor ((fromIntegral x)**(1/4))

pe87 = size $ fromList $ primePowerTriples 50000000
--pe87 = length $ unique $ quicksort $ primePowerTriples 50000000
--pe87 = length $ primePowerTriples 50000000


-- 89
-- See pe5_89.hs


-- 91
-- Right triangles with integer coordinates: Given that (x0,y0) = (0,0) and 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?
-- Answer: 14234 (0.02 secs, 11885880 bytes)
-- 1) horizontal/vertical sides below/right of hypotenuse = n^2 (right angle at i,j below hypotenuse) - limit
-- 2) horizontal/vertical sides above/left of hypotenuse = n^2 (right angle at i,j above hypotenuse)
-- 3) horizontal/vertical sides below/left of hypotenuse = n^2 (right angle at 0,0)
-- 4) hypotenuse on x-axis (0 degrees) or rotated CCW (< 90) with right angle above hypotenuse;
--      sides not horizontal or vertical = hypDn
-- 5) hypotenuse on y-axis (90 degrees) or rotated CW (> 0) with right angle below hypotenuse; 
--      sides not horizontal or vertical = hypDn (by symmetry)
-- 6) right angle at 0,0 sides inside x or y axis = 0

hypDn n = [(i,j, m*u, m*v) | i <- [1..n], j <- [1..n], let d = gcd i j, let u = j `div` d, let v = i `div` d, m <- [1.. (j `div` v)], i+u*m <= n]
pe91' n = 3*(n^2) + 2 * (length $ hypDn n)
pe91 = pe91' 50


-- 92
-- Square digit chains: How many starting numbers below ten million will arrive at 89?
-- Answer: 8581146 (32.57 secs, 6717408512 bytes)
-- From testing, it appears that most of the time is spent in sum $ map (^2) (digits n)
-- The new solution involves using obvious solution for all the numbers below 567 = 7 * 9^2,
-- then building the sum of the squares of all the permutations of a 7 digit number, and checking for
-- inclusion in the list of solutions below 567.
-- only ~14% of the numbers converge to 1 (v. 86% for 89), based on the slow answer.
-- Through testing it was determined that is it 50%+ faster to check against the shorter list,
-- even though 85% of the numbers will search the entire list before failing.
-- I will be checking all numbers from 0000000 to 9999999, zero is not relevant, but it since
-- it is not in a chain that ends in 1, it won't be counted anyway.
shortList = filter (not . chain89) [1..567]
            where chain89 n
                      | n == 89   = True
                      | n == 1    = False
                      | otherwise = chain89 (sum $ map (^2) (digits n))
digitSums = [t | a <- sqs, b <- sqs, c <- sqs, d <- sqs, e <- sqs, f <- sqs, g <- sqs, let t = a + b + c + d + e + f+ g]
            where sqs = map (^2) [0..9]
pe92 = (10^7 - 1)  - (length $ filter chainEndsIn1 digitSums) where
    chainEndsIn1 n
      | n `elem` shortList  = True
      | otherwise           = False


-- 94
-- Almost equilateral triangles: Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).
-- Answer: 518408346 (0.00 secs, 1065872 bytes); plus 0.48 seconds for sample
-- Analysis: We shall define an almost equilateral triangle (AET) to be a triangle for which two sides are equal and the third differs by no more than one unit.
--   I start with a brute force method, to check all upto 10^5 this reveals that a brute force solution will take too long,
--   but it does produce a well-known sequence, OEIS A120893, that can be generated much faster.
isIntArea s b = if (even b) then isps (s2 - b2') else isps' (4*s2 - b2)
  where s2  = s^2
        b2  = b^2
        b2' = b2 `div` 4
isps n = n'*n' == n where n' = floor $ sqrt (fromIntegral n)
isps' n = (n'*n' == n) && (n' `mod` 4 == 0) where n' = floor $ sqrt (fromIntegral n)
sample94 = [(s,b,s+s+b) | s <- [5..50000], b <- [(s-1),(s+1)], isIntArea s b]
-- OEIS A120893: a(n) = 3*a(n-1) + 3*a(n-2) - a(n-3); a(0)=1, a(1)=1, a(2)=5
hyp94 = let r = 1:1:5: zip3With (\a b c -> 3*(a+b) - c) (drop 2 r) (drop 1 r) r
        in r
pe94 = sum $ takeWhile (<10^9) [s+s+b | s <- (drop 2 hyp94), b <- [(s-1),(s+1)], isIntArea s b]


-- 96
-- See pe5_96.hs


-- 97
-- Large non-Mersenne prime: Find the last ten digits of 28433 * 2^7830457 + 1.
--    this is a massive non-Mersenne prime which contains 2,357,207 digits
-- Answer: 8739992577 (0.01 laptop secs, 1583960 bytes)
-- Note: (a  *b^c  +d) mod m == (a * (b^c mod m) + d) mod m 
-- we can solve b^c mod m with Modular exponentiation which is much more efficient when c is large.
pe97 = (28433 * (powerMod 2 7830457 (10^10)) + 1) `mod` 10^10


-- 98
-- See pe5_98.hs


-- 99
-- See pe5_99.hs


-- 100
-- Arranged probability: By finding the first arrangement to contain over 10^12 discs in total, determine the number of blue discs that the box would contain.
-- Answer: 756872327473 (0.01 secs, 5748152 bytes)
-- Analysis: P(BB) = b(b-1)/(t(t-1)) = 1/2 where b = number of blue discs, t = total number of discs,
--   and P(BB) is the probability of drawing two blue discs.  Find b and t which equal exactly 1/2.  rearranging terms and expanding, 2B^2 - 2B + T - T^2 = 0.
--   This equation is true for B,T = 15,21 and 85,120.  It can be shown that the equation holds for these values, and not others nearby.
--   using the quadratic formula, to solve for B if T is known  aB^2 +bB + c == 0, where a = 2, b = -2, c = T-T^2
--   b t = (0.5,0.5*(sqrt (1 - 2*t*(1-t))))  so 1 - 2t*(1-t) must be a perfect square
--   isps n = n'*n' == n where n' = floor $ sqrt (fromIntegral n)
--   blue t = (1+ (isqrt (1 - 2*t*(1-t)))) `div` 2
--   smallAnswers = map (\(t,_) -> (blue t,t)) $ filter (\(_,t') -> isps t') [(t,1-2*t*(1-t)) | t <- [10..(10^6)]]
-- smallAnswers => [(15,21),(85,120),(493,697),(2871,4060),(16731,23661),(97513,137904),(568345,803761)] in (2.87 secs, 1043295932 bytes)
-- and the following generated a correct solution for 10^3, however failed to find a solution after 10 hours with 10^12:
-- pe100 = blue $ fst $ head $ filter (\(t,t') -> isps t') [(t,1-2*t*(1-t)) | t <- [(10^3)..]]
-- Since the inner loop has some simple math, and the isPerfectSquare solution, the only optimization is at isPerfectSquare
-- Using a very complicated solution (http://stackoverflow.com/questions/295579/fastest-way-to-determine-if-an-integers-square-root-is-an-integer)
-- it is possible to get a 35% increase in speed which will never be enough.
-- I need a better way to get to zoom in to the solution.
-- After graphing f(x) = 1-2*x*(1-x) - floor(sqrt(1-2*x*(1-x))), I could see there was a pattern, but I could not 
-- deduce it, so I decided to check OEIS for the sequence 21,120,697,4060,23661,137904,803761, which turns out to be
-- sequence A046090, similarly, 15,85,493,2871,16731,97513,568345 is sequence A011900, whose formula is b(n)=6*b(n-1)-b(n-2)-2, with b(0)=1, b(1)=3 
-- on that site I also learned that if I let t=(T+1)/2, and b=(B+1)/2 then 2b(b-1) = t(t-1) can be rewritten
-- as B^2 -2T^2 = 1 which is Pell's Equation, which can also be solved quickly 
-- A011900: a(n)=6*a(n-1)-a(n-2)-2, with a(0)=1, a(1)=3
blues = 1 : scanl (\b a -> 6*b-a-2) 3 blues
-- using the quadratic formula to solve  t(t-1) = 2b(b-1) for t(b) we have:
total blue = (1+isqrt(1+8*blue*(blue-1))) `div` 2
pe100 = fst $ head $ dropWhile (\(_,t) -> t < 10^12) [(blue, total blue) | blue <- blues]

