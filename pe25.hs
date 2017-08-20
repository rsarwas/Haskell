-- Project Euler in Haskell problems 281..500

import ProjectEuler


-- 493
-- Under the Rainbow: 10 balls in 7 colors (70 balls) are placed in an urn. What is the expected number of distinct colors in 20 randomly picked balls?
-- Answer:
-- Analysis:

--prob' n = [((d-n), d) | (n,d) <- prob n]
--prob n = zip (num n) (denom n)
--num n = scanl (*) 2 (reverse [0..(n-1)])
--denom n = scanl (*) 2 (reverse [1..((n*2)-1)])

ev2' c n = ev2 n !!(c-1)
ev2 n = [a/c + 2*b/c | (a,b,c) <- prob2 n]
prob2 n = [(n', d-n', d) | (n',d) <- zip (num n) (denom n)]
  where
    num n = scanl (*) 2 [n-1,n-2..0]
    denom n = scanl (*) 2 [n2-1,n2-2..1]
    n2 = n*2


-- 500
-- Problem 500!!!
-- Find the smallest number with 2^500500 divisors. Give your answer modulo 500500507
-- Answer:
{-
Analysis:  The divisors of a number are multiples of primes.  i.e. {p1^e1, ... pn^e1}
The number of divisors is the number of power sets of p1..pn.  The total number of
divisors = (e1+1)*(e2+1)*..*(en+1).  I.e.  30 has 2,3,5 as prime factors (one each)
the number of divisors is (1+1)(1+1)(1+1) = 2^3 = 8 divisors (1,2,3,5,6,10,15,30)
It is convenient the the number of divisors is power of 2.  One solution is the
first 500500 prime numbers, however, it can be seen that since after 6 (2*3) with
d(n) = 2^2, we could add 5 for n = 30; d(n) = 2^3  or 2,2,2,3 = 18 d(n) = (3+1)(1+1) = 2^3
We are looking for the smallest number so 18 beats 30.  We know to add 2,2 instead of
5 because 4 < 5.  The algorithm is to pick the first prime, and add its square to
a list of squares, then pick (pop) the smaller of the list of primes or the list
of squares.  That number is added to the end of the list of squares, repeat 500500 times.
The number will be monsterously large, so we modulo at each step.  This is valid
since a*b*c mod n  = ((((a mod n) *b) mod n) *c) mod n.
This is easy to express in a for loop with a mutable squares list - See pe500.py.
-}
