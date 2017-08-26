-- Project Euler in Haskell problems 81..100

import ProjectEuler
import Data.Set (fromList, size)  -- for uniquing in 87
import Data.Ratio -- for 93
import Data.List -- 88, 93
import System.Random -- 84
import qualified Data.Map.Strict as Map -- 84, 95

-- 81
-- See pe5_81.hs


-- 82
-- See pe5_82.hs


-- 83
-- See pe5_83.hs

-- 84
-- Monopoly
-- Answer: 101524 (0.94 secs, 325,983,680 bytes)

-- Fisher Yates randomizer for shuffling cards
fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)

shuffle :: Int -> [a] -> [a]
shuffle seed = fst . fisherYates (mkStdGen seed)

roll :: Int -> Int -> [(Int,Int)]
roll sides seed = zip (dicerolls sides seed) (dicerolls sides (seed +1))
  where
    dicerolls sides seed = randomRs (1, sides) (mkStdGen seed)


nextRR :: Int -> Int
nextRR sq
  | sq == 7   = 15
  | sq == 22  = 25
  | otherwise = 5

nextUT :: Int -> Int
nextUT sq = if sq == 22 then 28 else 12

playCH :: Int -> Int -> [Int] -> Int
playCH i sq cards
  | i == cards!!1  = 0   --GO
  | i == cards!!2  = 10  --JAIL
  | i == cards!!3  = 11  --C1
  | i == cards!!4  = 24  --E3
  | i == cards!!5  = 39  --H2
  | i == cards!!6  = 5   --R1
  | i == cards!!7  = nextRR sq
  | i == cards!!8  = nextRR sq
  | i == cards!!9  = nextUT sq
  | i == cards!!10 = sq - 3  -- if sq == 36, then we need to play the CC
  | otherwise      = sq

playCC :: Int -> Int -> [Int] -> Int
playCC i sq cards
  | i == cards!!1 = 0   --GO
  | i == cards!!2 = 10  --JAIL
  | otherwise     = sq

-- state = (square[0:39], chanceIndex[0:15], commChestIndex[0:15], doublesCount[0:2]
-- roll = (die1, die2)
move :: (Int, Int, Int, Int, [Int], [Int]) -> (Int, Int) -> (Int, Int, Int, Int, [Int], [Int])
move (sq, ch, cc, dbl, chCards, ccCards) (d1, d2)
  | d1 == d2 && dbl == 2                = ret 10 ch cc 0
  | nsq == 30                           = ret 10 ch cc ndbl
  | nsq == 2 || nsq == 17 || nsq == 33  = ret playcc ch (draw cc) ndbl
  | nsq == 36                           = let nsq' = playch in if nsq' == 33 then (ret playcc (draw ch) (draw cc) ndbl) else (ret nsq' (draw ch) cc ndbl)
  | nsq == 7 || nsq == 22               = ret playch (draw ch) cc ndbl
  | otherwise                           = ret nsq ch cc ndbl
  where
    ret a b c d = (a, b, c, d, chCards, ccCards)
    playcc = playCC cc nsq ccCards
    playch = playCH ch nsq chCards
    playcc' = playCC cc 33 ccCards
    nsq = (sq + d1 + d2) `mod` 40
    ndbl = if d1 == d2 then dbl + 1 else 0
    draw i = (i + 1) `mod` 16

top3 sides n seed = map snd $ take 3 $ reverse $ quicksort [(y,x) | (x,y) <- histogram]
  where
    chCards = shuffle seed [0..15]
    ccCards = shuffle (seed +1) [0..15]
    movelist = scanl move (0, 0, 0, 0, chCards, ccCards) (roll sides (seed+2))
    playgame = take n $ drop 1 $ movelist
    histogram = zip [0..39] (counts [0..39] [x | (x,_,_,_,_,_) <- playgame])
test84 = top3 6 100000 5345  -- 102400
pe84 = top3 4 100000 683475

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


-- 86
-- Cuboid route: Find the least value of M such that the number of solutions first exceeds one million.
-- Answer: 1818 (0.21 secs, 18760528 bytes) + (0.07 secs, 6276728 bytes) + last bit of search/verification by hand
-- Analysis:  the solution is the shortest "straight" integer path along the surface of an integral cuboid.
--   When the cuboid is unfolded, the path is the hypotenuse of a right triangle, so this is a pythaorean triple problem.
--   the sides a and b are one of the following choices (h,w+l), (w,h+l), (l,w+h). and a little pen an paper work shows
--   that for any pythagorean triple, the number of unique cuboids is (a `div` 2) + (b `div` 2), however when the long leg
--   is divided up on two surfaces, then the integral length is not the shortest route.  This happens when one of the segments
--   of the long leg is greater than the short leg.  i.e for the triple 6,8,10 on a cuboid of 6,7,1 then the shortest route is
--   on the hypotenuse formed by 7 and 6+1.  There are a-b-1 of these non-integral shortes routes where a is the longer leg.
--   all the remaining fit on a cuboid with max dimension of b where b is the short leg.  This makes it tricky to maintain a
--   running tota, so I will look at long legs and short legs separately, and add them as a last manual step.
baseLegsTo m' = [longLegFirst m n | n <- [1..nmax], m <- [(n+1)..(mmax n)], odd (m-n), gcd m n == 1]
  where mmax n = isqrt (m' + n^2)
        nmax = isqrt (m' `div` 2)
        longLegFirst m n = ((max a b), (min a b))
          where a = m^2 - (n^2)
                b = 2*m*n
manyLegsTo m' = [(a*k,b*k) | (a,b) <- (baseLegsTo m'), k <- [1..(m' `div` a)]]
sortedLongLegsTo m' = nub $ sort $ manyLegsTo m'
sortedShortLegsTo m' = nub $ sort $ map (\(a,b) -> (b,a)) (manyLegsTo m')
bigCuboidsTo m' = [(a,(b `div` 2)) | (a,b) <- sortedLongLegsTo m']
smallCuboidsTo m' = [(a,(max 0 ((b `div` 2) - (b-a-1)))) | (a,b) <- sortedShortLegsTo m']
totalBigCuboidsTo m' = scanl (\(a1,a2) (x,y) -> ((max a1 x),(a2+y)) ) (0,0) (bigCuboidsTo m')
totalSmallCuboidsTo m' = scanl (\(a1,a2) (x,y) -> ((max a1 x),(a2+y)) ) (0,0) (smallCuboidsTo m')
-- the number provided is the length of the long leg, so we need to go larger to ensure all the small legs are captured.
pe86 = takeWhile (\(a,b) -> a < 1820) $ dropWhile (\(a,b) -> a < 1815) $ totalSmallCuboidsTo 4000
pe86' = takeWhile (\(a,b) -> a < 1820) $ dropWhile (\(a,b) -> a < 1815) $ totalBigCuboidsTo 2000


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


-- 88
-- Product-sum numbers: What is the sum of all the minimal product-sum numbers for 2≤k≤12000?
-- Answer:
--Analysis:
{-
There is always a solution for k at 2*k*1^(k-2) = 2k = 2 + k + (k-2)*1, but this
might not be minimal, but at least if provides an upper bound.
We need to look at 3x[3,4..] < 2k, 4x[4,5,...] < 2k ... n*n < 2k; n = sqrt(2k)
as well as 2x2x[2,3,..] < 2k, 2x3x[3,4...], upto nxnxn < 2k where n = cuberoot(2k)
upto n terms where 2^n < 2k
-}
f2' k a = [t | x <- [3..(a `div` 3)], y <- [x..(a `div` x)], let t=x*y, t == x+y+k-2]
f2 k a = minimum (a:[t | x <- [3..(a `div` 3)], y <- [x..(a `div` x)], let t=x*y, t == x+y+k-2])
f3' k a = [t | x <- [2..(a `div` 4)], y <- [x..(a `div` (2*x))], z <- [y..(a `div` (x*y))], let t=x*y*z, t == x+y+z+k-3]
f3 k a = minimum (a:[t | x <- [2..(a `div` 4)], y <- [x..(a `div` (2*x))], z <- [y..(a `div` (x*y))], let t=x*y*z, t == x+y+z+k-3])

f1 k = f3 k (f2 k (2*k))
-- I now have a solution for a list of 3 divisors or less, but this strategy
-- does not scale well.  I can use this to create an uppoer bound to use on a more
-- general solution.

-- If I can make a list of divisor lists as follows
dLists _ = [
  (4,[[2,2,2,2],[2,2,2,3],[2,2,2,4],[2,2,2,5],[2,2,2,6],[2,2,3,3],[2,2,3,4]]),
  (5,[[2,2,2,2,2],[2,2,2,2,3],[2,2,2,2,4],[2,2,2,2,5],[2,2,2,3,3]])]
--, where fst element of the tuple is the number of divisors, upto 2^n < 12000
-- Then the general solution is expressed as:
pe88 k = minSolution k (f1 k) (dLists k)

minSolution k limit ds = minimum [checklists k limit n l | (n,l) <- ds]
  where
    checklists k limit n l = min' limit
       [p | ds <- l, let p = product ds, let s = sum ds, p == s+k-n, p < limit]
    min' :: (Integral a) => a -> [a] -> a
    min' x [] = x
    min' x (y:ys) = min x (min' y ys)

-- Now I just need to figure out how to make the dlists
-- the solution considered so far is a recursive datatype without a base case
--makeDivisorList _ 0 = [[]]
--makeDivisorList limit n = map (x:) [(makeDivisorList limit' (n-1)) | x <- [2..(limit `div` (2^(n-1)))], let limit' = limit `div` x]


-- 89
-- See pe5_89.hs


-- 90
-- Cube digit pairs: How many distinct arrangements of the two cubes allow for all of the square numbers to be displayed?
-- Answer: 1217 (0.12 secs, 49853112 bytes)
-- Analysis: The search space is pretty small, so a brute force check of all options is fast enough.  The biggest trick is
--   to not double count d1,d2 different from d2,d1
options = [[a,b,c,d,e,f] | a <- [0..4], b <-[(a+1)..5], c<-[(b+1)..6], d<-[(c+1)..7], e<-[(d+1)..8], f<-[(e+1)..9]]
options' = map add6 $ map add9 options
  where add6 xs = if (9 `elem` xs && not (6 `elem` xs)) then 6:xs else xs
        add9 xs = if (6 `elem` xs && not (9 `elem` xs)) then 9:xs else xs
goodPair xs ys = (check xs ys 0 1) && (check xs ys 0 4) && (check xs ys 0 9) && (check xs ys 1 6) && (check xs ys 2 5) &&
                 (check xs ys 3 6) && (check xs ys 4 9) && (check xs ys 6 4) && (check xs ys 8 1)
check xs ys x y = (x `elem` xs && y `elem` ys) || (x `elem` ys && y `elem` xs)
pe90 = sum [1 | d1 <- options', d2 <- dropWhile (/= d1) options', goodPair d1 d2]


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
-- Answer: 8581146 (10.066 secs - real time for version compiled with ghc -O)
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


-- 93
-- Arithmetic expressions
-- Answer: 1258 (10.03 secs, 5989995008 bytes); using Maybes slowed it down to 13.60 secs
-- Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers, 1 to n,
-- can be obtained by combined the integers with any combination of the four operators (*./,+,-).
-- giving your answer as a string: abcd.
-- Analysis: There are 126 unique sets of the first 4 integers 1234 .. 6789 with 4*3*2*1=24 permutations in ordering. combined with
-- 4^3 = 64 permutations of the operators, in a 4 different postfix configurations.  This is 24*64*4 =  6144 expressions for 126
-- sets of numbers. Therefore we must solve 774144 expressions.  This is a fairly manageable search
-- space, so it should be possible to solve this with brute force.
-- I will use RPN to generate all the possible combinations of integers and operators for each set of numbers.
-- A data.ratio will be used to store intermediate results, and if the result is not an integer, then 0 will be returned
-- The only characters allowed as input are "123456789-+*/"

ord :: Char -> Ratio Int
ord c = (fromEnum c - fromEnum '0') % 1
chr :: Int -> Char
chr i = toEnum ((fromEnum '0') + i)

rpn _ (Nothing:_:s) = Nothing:s
rpn '+' (Just a:Just b:s) = Just (b + a):s
rpn '*' (Just a:Just b:s) = Just (b * a):s
rpn '-' (Just a:Just b:s) = Just (b - a):s
rpn '/' (Just a:Just b:s) = if (a == 0) then Nothing:s else Just (b / a):s
rpn c s = if ('1' <= c && c <= '9') then Just (ord c):s else Nothing:s

rpnCalculator :: [Char] -> Maybe Int
rpnCalculator s = checkSolution (solve s)
  where solve exp = head $ foldr rpn [] (reverse exp)
        checkSolution Nothing = Nothing
        checkSolution (Just a)  = if (denominator a) == 1 then (let n = numerator a in if n < 1 then Nothing else Just n) else Nothing

nsets :: [[Int]]
nsets = [[a,b,c,d] | a <- [1..6], b <-[(a+1)..7], c<-[(b+1)..8], d<-[(c+1)..9]]
opsets :: [[Char]]
opsets = [[a,b,c] | a <- ops, b <- ops, c <- ops] where ops  = "*+-/"

rpnStrings [n1,n2,n3,n4] [op1,op2,op3] = [[n1, n2, op1, n3, op2, n4, op3],
                                          [n1, n2, n3, op1, op2, n4, op3],
                                          [n1, n2, n3, op1, n4, op2, op3],
                                          [n1, n2, n3, n4, op1, op2, op3]]

allTargets nset = [rpnCalculator exp | nsetperm <- [lexiPerm i nset | i <- [0..23]], opperm <- opsets, exp <- (rpnStrings (map chr nsetperm) opperm)]
targets nset = sort $ nub $ map (\(Just a) -> a) $ filter (/=Nothing) (allTargets nset)
targetCount ns = (snd $ head $ dropWhile (\(x,i) -> i == x) (zip ns [1..])) - 1
pe93 =  snd $ last $ sort $ [(targetCount (targets nset), (digitsToInt nset)) | nset <- nsets]


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


-- 95
-- Amicable chains
-- Find the smallest member of the longest amicable chain with no element exceeding one million.
-- Answer: 14316  in 13.150s unix real time compiles with ghc -O2
{- Analysis:
First step is to create a map between the first 1 000 000 numbers and the sum of
thier properDivisors.  I have a divisor function in the ProjectEuler library, and
properDivisors = init . divisors, however, it is really slow.  Liam suggested, I
just do a brute force divisor check on each number and that is much faster.  It
still needs to be compiled, but at least I can get the divisors in under 30 sec.
Next step:
recursively lookup the values from the map.  Since not all numbers are in the map
i.e. primes and sums greater than 10^6, I am hoping that most chains will terminate
quickly.
The following works for finding 220 -> 284 -> 220, find hits a stack overflow
with 1600, meaning that it probably hit a loop  i.e.  x -> ... -> 220
I have to remove all items from the map after I have check them.  This will speed
up the later lookups (causing many chains to terminate more quickly, since we know
how they will terminate), and it prevents falling into an endless loop.
-}
divisorMap :: Int -> Map.Map Int Int
divisorMap n = Map.fromList [(k,v) |
   k <- [2..n], let v = sum $ properDivisors k, k /= v, v <= n, v /= 1]

properDivisors :: Int -> [Int]
properDivisors n = 1:firstHalf ++ secondHalf
  where
    divs = [(q,q1) | q <- [2..(isqrt n)], let (q1,r) = n `quotRem` q, r == 0]
    firstHalf = map fst divs
    -- get the quotients, but ignore the square roots (do not double count)
    -- reverse it so divisors are in order (not really required)
    secondHalf = map snd $ reverse $ filter (uncurry (/=)) divs

pdTest :: Int -> [(Int,[Int])]
pdTest n = [(k,properDivisors k) | k <- [2..n]]

-- The elements must be searched smalest to largest to ensure the smallest
-- number in the loop is returned
-- input and return tuple is (length, smallest, map)
maxLoopLength :: (Ord a) => (Int, a, Map.Map a a) -> a -> (Int, a, Map.Map a a)
maxLoopLength (lo, xo, mx) x
  | lo < l' = (l',x',mx')
  | otherwise = (lo,xo,mx')
  where (l',x',mx') = findLoopLength x mx

-- returns the length of the loop starting at x if found in map mx,
-- and the smallest number in the loop, as well as the revised map
findLoopLength :: (Ord a) => a -> Map.Map a a -> (Int, a, Map.Map a a)
findLoopLength x mx = (l, x', mx')
  where
    l = lastLoopLength (x:c)
    x' = if l == 0 then x else smallestInLoop
    (c, mx') = findChain x mx
    smallestInLoop = minimum $ take l (reverse c) -- omit x, since it is a dup

findChain :: (Ord a) => a -> Map.Map a a -> ([a], Map.Map a a)
findChain x mx =
  case x' of {Nothing -> ([],mx'); Just v -> (v:c,mx'') where (c,mx'') = findChain v mx'}
  where (x',mx') = popMap x mx

-- lookup an element in a map, and return the map without that element
-- since the element may not be in the map, a Maybe is returned
popMap :: (Ord a) => a -> Map.Map a a -> (Maybe a, Map.Map a a)
popMap x mx = (v,mx')
  where
    v = Map.lookup x mx
    mx' = Map.delete x mx

-- The remainder can be done faster with Data.Maybe, and or Data.List

-- finds the distance (in indices) between the last element and it earlier dup
-- i.e. [1,2,3,1] == 3; [2,1,2,3,4,2] == 3; [1,2,3,4] == 0
lastLoopLength :: (Eq a) => [a] -> Int
lastLoopLength xs = case reverse xs of {[] -> 0; h:xs' -> firstIndexOrZero h xs'}

firstIndexOrZero :: (Eq a) => a -> [a] -> Int
firstIndexOrZero x xs = firstOrDef 0 $ elemIndices x xs

firstOrDef :: a -> [a] -> a
firstOrDef d xs = case xs of {[] -> d; i:_ -> i}

-- In Data.List
--elemIndices :: (Eq a) => a -> [a] -> [Int]
--elemIndices x = findIndices (x==)

--findIndices :: (a -> Bool) -> [a] -> [Int]
--findIndices p xs = [i | (x,i) <- zip xs [1..], p x]

pe95' :: Int -> Int
pe95' n = smallest
  where
    (_,smallest,_) = foldl maxLoopLength (0,0,ds) [1..n]
    ds = divisorMap n
pe95 :: Int
pe95 = pe95' (1000000-1)


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


main :: IO ()
main = do
  print pe95
