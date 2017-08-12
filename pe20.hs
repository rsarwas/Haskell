-- Project Euler in Haskell problems 381..400

import ProjectEuler

-- 387
-- Harshad Numbers: Find the sum of the strong, right truncatable Harshad primes less than 10**14
-- Answer: 696067597313468 (2173.39 secs, 1,847,476,563,064 bytes)

-- There is a lot of duplicate work in this algorithm: harshad' 11 takes < 2 sec, but
-- shs $ harshad' 11 takes ~ 40 sec; and they do much the same; except the prime check
-- Maybe there is a way to implicitly determine primeness by the structure.
-- Note replacing the isPrime with odd runs in 5.67 seconds.  A faster prime check would help

-- harshad - Generates the list of larger Harshad numbers that are right truncatable to
-- the given Harshad numbers. This is done by appending 0..9 to each number in the list
-- and returning those that are Harshad numbers.
harshad :: [Int] -> [Int]
harshad a = [n | x <- a, y <- [0..9], let n = 10*x+y, p n]
  where p n = n `rem` (sum $ digits n) == 0

-- applies the harshad function n times
-- yields all right truncatable Harshad numbers below 10**(n+1)
harshad' :: Int -> [Int]
harshad' n = ((!! n) . iterate harshad) [1..9]

-- phs - prime Harshad numbers. Given a list of numbers, appends a digit, and returns
-- the larger number if it is prime. i.e. returns the prime numbers which when the last
-- digit is truncated the truncated number is in the provided list
phs :: [Int] -> [Int]
phs xs = filter (isPrime) [h*10+x | h <- xs, x <- [0..9]]

-- shs - strong Harshad numbers.  A Harshad number is strong if the number divided
-- by the sum of the digits is prime.
shs :: [Int] -> [Int]
shs = filter (\x -> isPrime $ x `div` (sum $ digits x))

-- sh; sum of the strong, right truncatable Harshad primes less than 10**n
-- none of the single digit Harshads numbers are strong, because 1 is not prime,
-- therefore we start with 1 and not 0
sh :: Int -> Int
sh n = sum [ sum $ phs $ shs $ harshad' x | x <- [1..(n-2)]]

pe387 = sh 14
