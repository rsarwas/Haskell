-- Project Euler in Haskell problems 121..140

import ProjectEuler
import Data.Ratio
import Data.List

-- 121
-- Disc game prize fund: Find the maximum prize fund that should be allocated to a single game in which fifteen turns are played.
-- Answer: 2269 (0.07 secs, 30,257,960 bytes)
-- Analysis: from the example, the prize fund is the integer inverse of the odds
--  In the first round the odds are 1 in 2 of getting a blue.  In the second round, the odds are 1 in 3, etc ...
--  Now you list all ways that you can form a winning game, and sum over the products of the corresponding probabilities.
--  For example in a game with 3 rounds, the odds of getting a blue in each round (one way to win) is 1/2*1/3*/14
--  The game can also be one by drawing 1 red chip this can happen in 3 ways (in each round)  The odds of drawing a red
--   disc in the first round is 1/2, in the second: 2/3, third: 3/4, etc...  the probabilities for these three options are
--   1/2*1/3*1/4 (first round), 1/2*2/3*1/4 (second round), and 1/2*1/3*3/4 in the third round.  This can be simplified by
--   extracting a = 1/2*1/3*1/4 from each case, then we have a + a +2a + 3a (for three rounds).
--   as the number of rounds grows, the combinations of way to wins grows. in n rounds, you can draw 0 to n - (n `div` 2) - 1 red
--   discs. There are n choose r ways to draw a red disc and win. (there is also the case of drawing no red disc).  If each of the
--   if the list of rounds [1..n] is the list drawn from, then the product of the selected terms from that is the factor that a is
--   multiplied against in the example above.
oddsNoRed n = product [ x+1 | x <- [1..n]]
bluesNeeded n = (n `div` 2) + 1
allowableReds n = n - (bluesNeeded n)
redFactors n = concat [ map product (nChoose r [1..n]) | r <- [1..(allowableReds n)]]
payout n = (oddsNoRed n) `div` (1 + (sum (redFactors n)))
pe121 = payout 15
-- FIXME - there is a conversion problem going on here that I could not resolve.
-- for values of 13 and greater, the first term is an Integer, and the second is an Int,
-- for values of 15 and greater, both terms should be Integers, but are being truncated to Ints
--  The following produces the correct result when run in ghci.
-- let a = 1 + (sum $ concat [ map product (nChoose r [1..15]) | r <- [1..7]])
-- let b = product [2..16]
-- b `div` a


-- 124
-- Ordered radicals: If rad(n) is sorted for 1 ≤ n ≤ 100000, find E(10000), where rad(n) is the product of distinct prime factors of n
-- Answer: 21417 (16.13 secs, 5,653,048,224 bytes)
-- Analysis: simple brute force solution, using functions already written for other solutions.
--           since the primeFactors are already sorted, I can use my existing unique function to remove dups
--           Since the sqrt 100000 < 10,000, we need to evaluate all numbers.
--           I'm sure there is a trick, or optimization here, but I'm under a minute, so it can wait.
--           note that getting the primeFactors of the 100,000 numbers is 14 of the 24 seconds
pe124 = snd ((quicksort $ [(r,n) | n <- [1..100000], let r = rad n, r < 10000]) !! 9999)
      where
        rad n = product $ unique $ primeFactors' n p
        p = primesTo (isqrt 100000)

-- 125
-- Palindromic sums: Find the sum of all the numbers less than 10^8 that are both palindromic and can be written as the sum of consecutive squares.
-- Answer: 2906969179 (8.11 secs, 3,660,998,264 bytes)
{--
Analysis:
note: A sum has to have at least two terms
      while this is obvious by the definition of sum, some texts on partitions, consider a
      single term to be a sum.  If this were the case, then 1,4,9,121,484 (which equal
      1^2, 2^2, 3^2, 11^2 and 22^2 respectively) would be sum of squares palindromes below 1000.
      There are 11 palindromes below 1000 (whose sum is equal to 4162) without considering these terms.
note: Only positive numbers are considered; ignore 0^2 as a term.

The solution is the sum all palindromes that are sums of consecutive squares less than n
a palindrom is a number which equals the number with its digits in reverse order
The terms in s1 .. s(m-1) are ALL the sums of consecutive squares less than n
NOTE: it is possible that a number can be formed by more than one sum of squares
where
  s1 = [1^2 + 2^2, 1^2 + 2^2 + 3^2, ... 1^2 + 2^2 + .. (m-1)^2 + m^2]
  s2 = [2^2 + 3^2, 2^2 + 3^2 + 4^2, ... 2^2 + 3^2 + .. (m-1)^2 + m^2]
   :
  s(m-1) = [(m-1)^2 + m^2]

if m is the first integer for which m^2 + (m+1)^2 > n then
1) I do not need to consider any additional terms in these sequences
2) I do not need to consider any additional sequences
proof of 1:
Since 1,2,3,..m-1,m,m+1, are monotonically increasing, the squares of these
terms are monotonically increasing.  Therefore the terms in the sequences are
monotonically increasing.  If i is the index of the first term greater than n
then all terms at index i or greater exceed n.
The last term in each sequence is greater than or equal to (m-1)^2 + m^2
If there were a next term in each sequence it would be greater than or equal
to m^2 + (m+1)^2. Since m^2 + (m+1)^2 > n, I can ignore that next term and all
subsequent terms.
proof of 2:
The next sequence would be sm and the first term of sm would be m^2 + (m+1)^2
since m^2 + (m+1)^2 > n, I can ignore that term. and by proof 1, all subsequent terms
the sequence sm is empty. Therefore sm-1 is the last non-empty sequence.

find m:
let a = m-1, b = m and c = m+1
since a < b and b < c, we know a^2 + b^2 < b^2 + b^2 < b^2 + c^2;
if b^2 + b^2 = n then a^2 + b^2 < n and n < b^2 + c^2
solve for b in  b^2 + b^2 = n  => 2b^2 = n => b^2 = n/2
b = sqrt(n/2)  (unless n is a square, b will be irrational),
therefore  m = floor b

Initial run yielded 2916867073; which is the wrong answer. :(

After careful inspection I discovered that both 554455 and 9343439 can be
formed by two different sums.  Therefore I need to unique (with nub) the list
of palindromeSums before summing
--}

pe125 = sum $ nub $ palindromeSums (10^8)
palindromeSums n = filter palindrome $ concat $ sums' n
palindrome x = x == (digitsToInt $ reverse $ digits x)
-- need to drop the first item in the list since it is not a sum
sums' n = map (takeWhile (<n) . (drop 1)) $ sums n
sums n = map (scanl1 (+)) $ squares n
squares n =  [[x^2 | x <- [firstTerm..m]] | firstTerm <- [1..(m-1)]]
  where m = floor $ sqrt ((fromIntegral n)/2)
