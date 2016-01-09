-- Project Euler in Haskell problems 301..320

import ProjectEuler

-- 301
-- Nim: For how many positive integers n <= 2^30 does X(n,2n,3n) = 0?
-- Answer: 2178309  (3.46 secs, 1,465,718,040 bytes)
-- Analysis: Based on the wikipedia analysis of the game of Nim, X(a,b,c) = 0 if
-- a xor b xor c = 0.  since b is 2a, this is a left bit shift of 1 place, c = 3a, which
-- is 2a + a, in analyzing this binary math, and doing some simple examples, it became
-- clear that the result was non zero if and only if there were two consecutive 1's digits
-- in a.
-- nap = Non Adjacent Permutations, it is the number of ways that x binary 1's can be put
-- into a number with n binary digits, such that there are none of the 1s are adjacent.
nap :: Int -> Int -> Int
nap x n
  | n < 1       = error "There must be a positive number of digits"
  | x < 0       = error "There must be a positive number of digits"
  | x == 1      = n
  | n < (2*x-1) = 0
  | otherwise   = sum [nap (x-1) y | y <- [1..(n-2)]]

all non adjacent permutation for less than n digits
allnap :: Int -> Int
allnap n = sum [nap x n | x <- [1..((n +1) `div` 2)]]
pe301 = 1 + allnap 30 -- +1 for the = part of the <=