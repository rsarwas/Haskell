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
