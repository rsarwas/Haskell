module ProjectEuler
( count
, counts
, digits
, digitsToInt
, divisors
, divisorCount
, fibs
--, group
, isPrime
, isqrt
, lexiPerm
, nChooseR
, nChoose
, pascalsTriangle
, pascalsTriangleRow
, powerMod
, powerSet
, primeFactors
, primeFactors'
, primesTo
, quicksort
, replace
, transpose
, triangleNumbers
, triangleNumber
, triangleNumbersFrom
, unique
, wordsWhen
, zip3With
) where

import Data.Bits -- for shiftR in powerMod

-- returns the count of an item in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- returns a list of frequency list (number of times each item in list1 appears in list2
counts :: Eq a => [a] -> [a] -> [Int]
counts vs xs = map (flip count xs) vs

-- Returns a list of decimal digits from an integral
digits :: Integral a => a -> [a]
digits x
     | x < 0     = digits (-x)
     | x < 10    = [x]
     | otherwise = digits (x `div` 10) ++ [(x `mod` 10)]

-- converts a list of decimal digits into an Integer
digitsToInt :: Integral a => [a] -> a
digitsToInt = foldl (\a d -> a*10+d) 0

-- Returns a list of all the divisors of a number
divisors :: (Integral a) => a -> [a]
divisors n = unique $ quicksort $ map product (powerSet (primeFactors n))

-- Returns the number of divisors a number has (includes 1 and the number)
-- uses the fact the number of divisors is (c1+1)*(c2+1)* ... * (ck+1)
-- where n = p1^c1 * p2^c2 * ... * pk^ck where pi are the prime factors
divisorCount :: (Integral a) => a -> Int
divisorCount n  = product $ map ((+1).snd) $ (group (primeFactors n))

-- an infite list of fibonaci numbers [1,1,2,3,5,8,13,..]
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- group a list of equatable items into an association list with thier value and count
-- could be done with a monad.  I should try that someday as an exercise
group :: (Eq a) => [a] -> [(a,Int)]
group [] = []
group (x:xs) = (x,1 + (length $ filter (==x) xs)):(group (filter (/=x) xs))

-- Predicate that tests if a number is prime
isPrime :: (Integral a) => a -> Bool
isPrime n
  | n < 2     = False
  | otherwise = n == head (primeFactors n)

--isqrt: integral square root, isqrt n returns the largest integer a such that a * a <= n
isqrt :: (Integral a) => a -> a
isqrt n = truncate (sqrt (fromIntegral n))

-- Returns the nth lexigraphic permutation of a list of monotonically increasing elements
-- n <- 0 .. (length of list)! -1
lexiPerm :: (Eq t) => Int -> [t] -> [t]
lexiPerm _ [] = error "Empty list"
lexiPerm 0 items = items
lexiPerm i items
    | i <  0 = error "The permutation index must be a positive number"
    | i >= m2 = error "There are not that many permutations for these items"
    | otherwise = d:(lexiPerm r [x | x <- items, x /= d])
        where
            (q,r) = i `quotRem` m1
            m1 = product [1..(l-1)]
            m2 = m1*l
            l =  length items
            d = items !! q

-- Returns the number of permutations in N choose R  n!/(r!(n−r)!) where r <= n; 1<=r; 1<=n
-- nChooseR :: a -> a -> a
nChooseR n r = (product [(r+1)..n]) `div` (product [1..(n-r)])

-- Returns a list of lists each sub list is r elements long, and contains all
-- permutations of choosing r elements from an input list n long; 1 <= r <= n
-- nChoose :: Int -> [a] -> [[a]]
nChoose r xs
  | r   <  1  = error "The number to choose must be a positive integer"
  | len <  r  = error "The number to choose must be less than or equal to the list length"
  | len == r  = [xs]
  | r   == 1  = [[x] | x <- xs]
  | otherwise = nChoose' r xs
  where len = length xs
        nChoose' r (x:xs) = map (x:) (nChoose (r-1) xs) ++ (nChoose r xs)

-- Returns pascals Triangle, it is an infinite triangle
pascalsTriangle :: [[Integer]]
pascalsTriangle = map pascalsTriangleRow [1..]

-- Returns the nth row of pascals triangle, the first row is 1, not 0
pascalsTriangleRow :: (Integral a) => a -> [a]
pascalsTriangleRow 1 = [1]
pascalsTriangleRow n = zipWith (+) (0:previousRow) (previousRow++[0])
  where previousRow = pascalsTriangleRow (n-1)

-- Returns b^e mod m   (see http://en.wikipedia.org/wiki/Modular_exponentiation)
powerMod :: (Integral a) => a -> Int -> a -> a
powerMod base exp modulo = pm_helper 1 base exp modulo where
  pm_helper acc b e m
    | e <= 0 = acc
    | otherwise = let acc' = if (e `mod` 2 == 1) then ((acc * b) `mod` m) else acc
                      exp' = e `shiftR` 1
                      base' = (b * b) `mod` m
                  in pm_helper acc' base' exp' m

-- Retuns the power sets (set of all combinations) for a set, i.e. [1,2] => [[], [1], [2], [1,2]]
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = tailps ++ map m tailps where
  tailps = powerSet xs
  m = (:) x

-- The prime factors of a number; returns an empty list for numbers < 2
primeFactors :: (Integral a) => a -> [a]
primeFactors n = primeFactors' n (primesTo (isqrt n))
primeFactors' n possiblePrimes
  | n < 2               = []
  | null possiblePrimes = [n]
  | r == 0              = nextPrime:(primeFactors' q possiblePrimes)
  | otherwise           = primeFactors' n (tail possiblePrimes)
  where nextPrime = head possiblePrimes
        (q,r) = n `quotRem` nextPrime

-- a finite list of prime numbers: [2,3,5,7..n]
-- Algorithm stolen from http://www.haskell.org/haskellwiki/Prime_numbers
primesTo :: (Integral a) => a -> [a]
primesTo m
  | m < 2 = []
  | otherwise = 2 : sieve [3,5..m]  where
    sieve []     = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p..m])
    minus (x:xs) (y:ys) = case (compare x y) of
               LT -> x : minus  xs  (y:ys)
               EQ ->     minus  xs     ys
               GT ->     minus (x:xs)  ys
    minus xs      _     = xs

-- uses the quicksort algorithm to sort a list of sortable items
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted

-- Returns a new string with a new substring replacing all old substrings in the input
replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new haystack
  | haystack == old = new
  | take l haystack == old = new ++ replace old new (drop l haystack)
  | otherwise = head haystack : replace old new (tail haystack)
  where l = length old

-- transpose a matrix (list of lists)
transpose :: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

-- Returns an infinite list of triangle numbers [1,3,6,10,15..]
triangleNumbers :: [Integer]
triangleNumbers = scanl1 (+) [1..]

-- Returns the nth triangle number
triangleNumber :: (Integral a) => a -> a
triangleNumber n = n*(n+1) `div` 2

-- Returns an infinite list starting with the nth triangle number
triangleNumbersFrom :: (Integral a) => a -> [a]
triangleNumbersFrom n = scanl (+) x [(n+1)..] where x = triangleNumber n

-- unique-ifies the input list; the input list must be sorted.
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique [a] = [a]
unique (x:xs)
  | x == head xs = unique xs
  | otherwise    = x:(unique xs)

-- similar to words in Prelude, this will split on p instead of Char.isSpace
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- zip 3 lists together with your own function
zip3With :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zip3With _ [] _ _ = []
zip3With _ _ [] _ = []
zip3With _ _ _ [] = []
zip3With f (x:xs) (y:ys) (z:zs) = (f x y z) : (zip3With f xs ys zs)
