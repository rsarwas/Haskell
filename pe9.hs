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