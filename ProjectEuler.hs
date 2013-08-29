module ProjectEuler
( divisors  
, fibs  
, isPrime  
, isqrt  
, powerSet  
, primeFactors
, primesTo
, quicksort
, unique  
) where 

divisors :: (Integral a) => a -> [a]
divisors n = unique $ quicksort $ map product (powerSet (primeFactors n))

-- an infite list of fibonaci numbers [1,1,2,3,5,8,13,..]
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- Predicate that tests if a number is prime
isPrime :: (Integral a) => a -> Bool
isPrime n 
  | n < 2     = False
  | otherwise = n == head (primeFactors n) 

--isqrt: integral square root, isqrt n returns the largest integer a such that a * a <= n
isqrt :: (Integral a) => a -> a
isqrt n = truncate (sqrt (fromIntegral n))

-- Retuns the power sets (set of all combinations) for a set
-- i.e. [1,2] => [[], [1], [2], [1,2]]
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

-- uses the quicksort algorithm to sort a list of comparable items
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted

-- a list of unique items; the input list must be sorted.
unique :: (Eq a) => [a] -> [a]
unique [] = []
unique [a] = [a]
unique (x:xs)
  | x == head xs = unique xs
  | otherwise    = x:(unique xs)
