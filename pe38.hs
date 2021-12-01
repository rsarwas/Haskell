-- Project Euler in Haskell problems 741..760

import ProjectEuler
import Data.Set

-- 757
-- Stealthy Numbers
{-
How many stealthy numbers are there that don't exceed 10^14?
Answer: 75737353 in about 8 minutes
Analysis:
If we let a = 1x, b = 2y, c = 2x, d = 1y
then a+b = c+d+1 => 1x+2y = 2x+1y+1 => y=x+1 for x={1...}
and 2x(x+1) for x={1...} are stealthy numbers (SN = a*b = c*d = 2xy)
The following are the first few stealthy numbers:
x=1, y=2 => SN=4 (2*1*2); (a=1, b=4, c=2, d=2 a+b=5, c+d=4, 4+1=5)
x=2, y=3 => SN=12 (2*2*3); (a=2, b=6, c=4, d=3 a+b=8, c+d=7, 7+1=8)
x=3, y=4 => SN=24 (2*3*4); (a=3, b=8, c=6, d=4 a+b=11, c+d=10, 10+1=11)
x=4, y=5 => SN=40 (2*4*5); (a=4, b=10, c=8, d=5 a+b=14, c+d=13, 13+1=14)
:

Similarly, if we let a=2x, b=3y, c=3x, d=2y
then a+b = c+d+1 => 2x+3y = 3x+2y+1 => y=x+1 for x={1...}
and 6x(x+1) for x={1...} are stealthy numbers (SN = a*b = c*d = 6xy)
The following are stealthy numbers in this pattern:
x=1, y=2 => SN=12 (6*1*2); (a=2, b=6, c=3, d=4 a+b=8, c+d=7, 7+1=8)
x=2, y=3 => SN=36 (6*2*3); (a=4, b=9, c=6, d=6 a+b=13, c+d=12, 12+1=13)
x=3, y=4 => SN=72 (6*3*4); (a=6, b=12, c=9, d=8 a+b=18, c+d=17, 17+1=18)

*Note* that the first SN (12) in the second set is also the second SN in the
first set, but that the second SN (36) in set 2 is not in the first set.
Aside from the first n-1 in set n being non unique, I could find no other
pattern for when a stealthy number would be non-unique.

This pattern for finding stealthy numbers can be generalized to

a = (i)x, b = (i+1)y, c = (i+1)x, d = (i)y  for i={1...}
then SNi = a*b = c*d = (i)(i+1)xy where x = n={1...} and y = n+1

or more generally: SN = { (i)(i+1)(n)(n+1) } for n={1...} for i={1...}

In psuedo code:
Note: Trying to find bounds on i and n below missed some numbers
decided to break when SN exceeded MAX
i_limit: # at smallest n (=i) SN = i(i+1)(i)(i+1) < MAX => i < sqrt(sqrt(MAX))
n_limit: # Nn(Nn+1)i(i+1) < MAX  =>  Nn < sqrt(MAX/i/(i+1))
for i = 1...  
    SN = (i)(i+1)(n)(n+1) # start at n=i to eliminate well known duplicates
    if SN > MAX break
    for n = (i+1)...
        SN = (i)(i+1)(n)(n+1)
        if SN > MAX break
        // somehow Uniquify the set of SN found (i.e. place in a Hashset)
-}

-- Note: My unique and Data.List.nub are very poor performers on large lists
-- use Data.Set.fromList and get the count of the resulting set.
-- 10^10 (2sec); 10^11 = 1843662 (7.8sec); 10^12 = 6414062 (28.7s); 
-- 10^13 = 22119902 (119s); guessing 10^14 will take 8 minutes
pe757 = length $ fromList $ concat [takeWhile (<max) [sn i n | n <- [i..]] | i <- [1..i_max]]
   where max = 10^13
         i_max = (isqrt (isqrt max)) -- + 1
         sn i n = i*(i+1)*n*(n+1)