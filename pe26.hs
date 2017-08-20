-- Project Euler in Haskell problems 501..520

import ProjectEuler

import qualified Data.Set (fromList, member) -- for problem 504


-- 504
-- Square on the Inside: How many quadrilaterals ABCD strictly contain a square number of lattice points for m = 100?
-- Maximum number of lattice points is 2m*m = 20,000  = sqrt(2)*m*m < (1.42m)^2
-- Answer: 694687 2m55s unix time when compiled with ghc -O
-- performance:
--   ghci: qc:  20,30,40,100 = 0.06,0.26,0.78,30;
--         csl = 1.9,10.2,33.5,?40min? (32x,39x,43x,?80x?
--  ghc -O: length qc 100 and squares takes 0.435s unix time

-- tc is the triangle count; the number of lattice points strictly within the triangle
-- tc' assumes that the short leg (h=height) is provided before the long leg (l = length).
-- This should aid in memoizing the results
-- This is done by calculating the integral height at each lattice point along the axis
-- by using the slope of the hypotenuse: c/step = rise/run = h/l => c = h*step/l;
-- we count back from the X intercept to the y axis.
-- The two axis are ignored, as are points along the hypotenuse (where h*step is
-- evenly divisible by l, i.e remainder ==0, we subtract the point on the hypotenuse)
tc' :: Int -> Int -> Int
tc' h l = sum [ if r == 0 then q-1 else q | step <- [1..(l-1)], let (q,r) = (h*step) `quotRem` l]

-- find a faster way to find the lattice points inside the h l triangle
-- it really should be about h*l/2,  with a little experimenting, I found this
-- to hold
tc2 h l = ((h-1)*(l-1) - pointsOnDiagonal h l) `div` 2
  where pointsOnDiagonal h l = 1 -- TODO: figure this part out

-- symmetry is used to calculate tc a b from tc b a
tc :: Int -> Int -> Int
tc h l
  | h < l     = tc' h l
  | otherwise = tc' l h

-- ac is the axis count - the number of latice points on the 4 axes.
ac :: Int -> Int -> Int -> Int -> Int
ac a b c d = 1 + (a-1) + (b-1) + (c-1) + (d-1)

-- ttc is the total triangle count - the number of latice points on the 4 triangles.
ttc :: Int -> Int -> Int -> Int -> Int
ttc a b c d = (tc a b) + (tc b c) + (tc c d) + (tc d a)

-- qc is the quad count; the total count of all lattice points strictly within the quad
qc :: Int -> [Int]
qc n = [ (ac a b c d) + (ttc a b c d) | a <- [1..n], b <- [1..n], c <- [1..n], d <- [1..n]]

-- csl is the number count of occurrences where the count of the lattice points in the
-- quad is a square number
-- This is the slowest part of the algorithm.  using as set (hash table) to speed the search.
csl :: Int -> Int
--csl n = length $ filter (\x -> x `elem` [s*s | s <- [1..(truncate ((sqrt 2)*n))]]) (qc n)
--  where squares = [s*s | s <- [1..(2*n)]]
csl n = length $ filter (\x -> x `Data.Set.member` squares) (qc n)
  where squares = Data.Set.fromList [s*s | s <- [1..142]]

test504 = csl 4
pe504 = csl 100
{-
main :: IO ()
main = do
  let pe504 = csl 100
  print pe504
-}
