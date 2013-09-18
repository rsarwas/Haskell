-- Project Euler in Haskell problems 61..80

import ProjectEuler

-- 62
-- Cubic Permutations: Find the smallest cube for which exactly five permutations of its digits are cube.
-- Answer: 127035954683 (13.29 secs, 1381871616 bytes)
find :: Int -> Int -> [([Int],[Int])] -> [Int]
find m n found = 
  let sortedDigits = quicksort $ digits (n^3)
      matches = findMatch sortedDigits found
      matching = if (null matches) then [] else (snd (head matches))
  in if ((length matching) == (m-1))
     then (n:matching)
     else (find m (n+1) ((sortedDigits, n:matching):found))
  where findMatch needle haystack = take 1 (filter (\a -> (fst a) == needle) haystack)
pe62 = (head $ quicksort (find 5 346 []))^3


-- 63
-- Powerful digit counts: How many n-digit positive integers exist which are also an nth power?
-- Answer (wrong): 21 (0.01 secs, 3683256 bytes)
pe63 = last $ takeWhile check [1..] where check x = x == (length $ digits (9^x))
-- I made an error in interpreting this problem as find the largest n (which is 21)
-- for any n, 10^n will have n+1 digits, n must be a positive integer.
-- for n = 1; [1..9]^1 = [1..9] all are 1 digit numbers, therfore there are 9 solutions with n = 1
-- for n = 2: [1..9]^2 = [1,4,9,16,..81] therefore there are 6 solutions for n = 2
-- for n = 3: [4..9]^3 = [64,125..??] therefore there are 5 solutions for n = 3
-- for n = 4: [5..9]^3 = [725,1296..??] therefore there are 4 solutions for n = 4
-- for n = 5: [6..9]^3 = [7776,16807..??] therefore there are 3 solutions for n = 5
-- continuing thus, there are 3@6, 2@7..10, and 1@11, we already know 9^21 is the last 21 digit number
-- Therefore the real Answer is 9+6+5+4+(3*2)+(2*4)+(1*11) = 49


-- 67
-- See pe4_67.hs


-- 79
-- Passcode derivation: Given that the three characters are always asked for in order, analyse the file
-- so as to determine the shortest possible secret passcode of unknown length.
-- Answer: 73162890 (solved manually)
-- by scanning the list manually, we can gather the following relationship
-- 1,2,3,6,7,8,9 < 0
-- 3,7           < 1 < 0,2,6,8,9
-- 1,3,6,7       < 2 < 0,8,9
-- 7             < 3 < 0,1,2,6,8,9
-- 1,3,7         < 6 < 0,2,8,9
--                 7 < 0,1,2,3,6,8,9
-- 1,2,3,6,7     < 8 < 0,9
-- 1,2,3,6,7,8   < 9 < 0
-- Therefore, 7 must be the first number, and 0 must be the last
-- eliminating them from all the list, we are left with 3 being second, and 9 being penultimate.
-- iterate thus twice more to get the answer.