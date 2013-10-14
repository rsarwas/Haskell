-- Project Euler in Haskell problems 121..140

import ProjectEuler
import Data.Ratio

-- 121
-- Disc game prize fund: Find the maximum prize fund that should be allocated to a single game in which fifteen turns are played.
-- Answer: 2269 (0.11 secs, 16966496 bytes)
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
