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

-- all non adjacent permutation for less than n digits
allnap :: Int -> Int
allnap n = sum [nap x n | x <- [1..((n +1) `div` 2)]]
pe301 = 1 + allnap 30 -- +1 for the = part of the <=


-- 315
-- Digital Root Clocks: difference between number of transitions for Max and Sams Clocks
-- They are both fed all the prime numbers between 10^7 and 2*10^7
-- root clock means continually summing digits, i.e 137 -> 11 -> 2

countSam :: Int -> Int
countSam x
  | x == 0 = 6
  | x == 1 = 2
  | x == 2 = 5
  | x == 3 = 5
  | x == 4 = 4
  | x == 5 = 5
  | x == 6 = 6
  | x == 7 = 4
  | x == 8 = 7
  | x == 9 = 6

-- Represent a digital digit as a binary number
-- bit 0 is top Horizontal
-- bit 1 is left top vertical
-- bit 2 is right top vertical
-- bit 3 is middle horizontal
-- bit 1 is left bottom vertical
-- bit 1 is right bottom vertical
-- bit 6 is bottom horizontal
-- bit 7 is always 0
  bitRep :: Int -> Int
  bitRep x
    | x == 0 = 119 -- 0b01110111 (skip 3)
    | x == 1 =  36 -- 0b00100100 (skip 0,1,3,4,6)
    | x == 2 =  93 -- 0b01011101 (skip 1,5)
    | x == 3 = 109 -- 0b01101101 (skip 1,4)
    | x == 4 =  46 -- 0b00101110 (skip 0,4,6)
    | x == 5 = 107 -- 0b01101011 (skip 2,4)
    | x == 6 = 123 -- 0b01111011 (skip 2)
    | x == 7 =  39 -- 0b00100111 (skip 3,4,6)
    | x == 8 = 127 -- 0b01111111 (skip none)
    | x == 9 = 111 -- 0b01101111 (skip 4)

countMax :: Int -> Int
countMax popCount $ xor (bitRep x) (bitRep y)

transitionsSam :: Int -> Int
transitionsSam x
  | x < 10 = 2 * countSam x
  | otherwise = 2 * sum (map countSam (digits x)) + transitionsSam (sum $ digits x)

transitionsMax :: Int -> Int
transitionsMax x = 0

transitionsDelta :: [Int] -> Int
transitionsDelta xs = sum $ map (\x -> transitionsSam x - transitionsMax x) xs

pe315 :: Int
pe315 = transitionsDelta [137]
