-- 1) get the last element in a list
-- Use the builtin library function
last1 = last

-- Use indexing and list length
last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

-- Use indexing and list length with guards
last2' :: [a] -> a
last2' xs
    | l == 0  = error "empty list"
    | otherwise = xs !! (length xs - 1)
    where l = length xs

-- Use recursion
last3 :: [a] -> a
last3 [] = error "empty list"
last3 [x] = x
last3 (_:xs) = last3 xs

-- First element in the reverse list
last4 x = head (reverse x)

-- 2) Find the next to the last (penultimate) item in a last item in a list
--    e.g. penultimate ['a'..'z'] -> 'y'

--Using builtins library functions
penultimate2 xs = last (init xs)

-- with indexing
penultimate1 xs = xs !! (length xs-2)

--with Recursion
penultimate [] = error "empty list"
penultimate [x] = error "singleton"
penultimate x = second rx
   where rx = reverse x
         second (u:v:_) = v
         
-- 3) find the kth element in a list
elementat :: Int -> [a] -> a
elementat _ [] = error "element is beyond end of list"
elementat 0 (x:_) = x
elementat n (x:xs) = elementat (n-1) xs

-- 4) find the length of a list
mylength :: [a] -> Int
mylength = foldl (\acc _ -> acc+1) 0

-- 5) reverse a list
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]


-- 6) flatten

-- 7) reduce sequential and equal elements to a single instance in a list
dedup :: (Eq a) => [a] -> [a]
dedup [] = []
dedup [a] = [a]
dedup (a:b:xs) | a == b    = dedup (b:xs)
               | otherwise = a : (dedup (b:xs))

-- 8) compress