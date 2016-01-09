-- Project Euler in Haskell problems 161..180

-- 162
-- How many hexadecimal numbers containing at most sixteen hexadecimal digits exist with all of the digits 0,1, and A present at least once?
-- Answer: 6377292 (in Hex = 0x614F4C)
hexCount :: Int -> Int
hexCount 3 = 4
hexCount n = 3 * (hexCount (n-1))
pe162 = hexCount 16


-- 169
-- returns a list of powers of 2 [1,2,4,8..]
powersOf2 :: Integral a => [a]
powersOf2 = [2^n | n <- [0..]]
-- returns the powers of 2 make n (aka the on bits of the binary rep of n)
-- n = sum $ map (2^) (bin n)
bin :: Integral a => a -> [Int]
bin n
  | n == 0    = []
  | otherwise = (length p - 1) : (bin (n - (last p)))
     where p = takeWhile (<=n) powersOf2

f (x:[])   n n' = n
f (a:b:[]) n n' = (b-a)*n + (n - n')
f (a:b:xs) n n' = n'' + (f (b:xs) n'' n)
  where n'' = (b-a-1)*n + (n - n')

s x = f l n 1
  where  l = reverse (bin x)
         n = 1 + (head l)

pe169 = s (10^25)


-- 173
-- Using up to one million tiles how many different "hollow" square laminae can be formed?
-- Answer: 1572729  (3.03 secs, 1,387,625,960 bytes)
-- Analysis:
-- Hole size 1,3,5,... -> 8,16,24,32,40,48,56,64,72,80,88,96,... tiles in a ring
-- Hole size 2,4,6,... -> 12,20,28,36,44,52,60,68,76,84,92,100, ... tiles in a ring
-- for n=100 tiles:
--     Single rings = 12 from first list + 12 from second list; take while x < n
--     Double rings = 5 from l1 + 5 from l2; take while x < n/2+4 (rings are n/2 + n/2-4), count is list length - 1
--     Triple rings = 3 + 2  take while x < max n/3+8 (rings n/3+8 + n/3 + n/3-8), count is list length - 2
--     Quad rings =  1 + 1 take while x < n/4+12; count = len - 3

laminaeCount':: Int -> Int -> Int
laminaeCount' r t =
  (length $ takeWhile (\x -> x <= maxRingSize r t) [12,20..]) - (r-1) +
  (length $ takeWhile (\x -> x <= maxRingSize r t) [ 8,16..]) - (r-1)
    where maxRingSize rings tiles = tiles `div` rings + 4*(rings-1)  -- 0 < rings <= tiles

laminaeCount:: Int -> Int
laminaeCount tiles = sum $ takeWhile (>0) [laminaeCount' rings tiles | rings <- [1..]]
pe173 = laminaeCount 1000000
