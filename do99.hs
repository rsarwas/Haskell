main = do
  putStrLn "Problem 1"
  putStrLn "myLast [1,2,3,4]"
  print $ myLast [1,2,3,4]
  putStrLn "myLast ['x','y','z']"
  print $ myLast ['x','y','z']
  putStrLn ""
  putStrLn "Problem 2"
  putStrLn "myButLast [1,2,3,4]"
  print $ myButLast [1,2,3,4]
  putStrLn "myButLast ['a'..'z']"
  print $ myButLast ['a'..'z']
  putStrLn ""
  putStrLn "Problem 3"
  putStrLn "elementAt [1,2,3] 2"
  print $ elementAt [1,2,3] 2
  putStrLn "elementAt \"haskell\" 5"
  print $ elementAt "haskell" 5
  putStrLn ""
  putStrLn "Problem 4"
  putStrLn "myLength [123, 456, 789]"
  print $ myLength [123, 456, 789]
  putStrLn "myLength \"Hello, world!\""
  print $ myLength "Hello, world!"
  putStrLn ""
  putStrLn "Problem 5"
  putStrLn "myReverse \"A man, a plan, a canal, panama!\""
  print $ myReverse "A man, a plan, a canal, panama!"
  putStrLn "myReverse [1,2,3,4]"
  print $ myReverse [1,2,3,4]
  putStrLn ""
  putStrLn "Problem 6"
  putStrLn "isPalindrome [1,2,3]"
  print $ isPalindrome [1,2,3]
  putStrLn "isPalindrome \"madamimadam\""
  print $ isPalindrome "madamimadam"
  putStrLn "isPalindrome [1,2,4,8,16,8,4,2,1]"
  print $ isPalindrome [1,2,4,8,16,8,4,2,1]
  putStrLn ""
  putStrLn "Problem 7"
  putStrLn "flatten (Elem 5)"
  print $ flatten (Elem 5)
  putStrLn "flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])"
  print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
  putStrLn "flatten (List [])"
--  print $ flatten (List [])
  putStrLn ""
  putStrLn "Problem 8"
  putStrLn "compress \"aaaabccaadeeee\""
  print $ compress "aaaabccaadeeee"
  putStrLn ""
  putStrLn "Problem 9"
  putStrLn "pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']"
  print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
  putStrLn ""
  putStrLn "Problem 10"
  putStrLn "encode \"aaaabccaadeeee\""
  print $ encode "aaaabccaadeeee"
{--
  putStrLn ""
  putStrLn "Problem "
  putStrLn ""
  print $ 
  putStrLn ""
  print $ 
--}

-- 1) Find the last element of a list.

myLast :: [a] -> a
myLast [] = error "There is no last element in an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

{-- Other solutions:
-- Use the builtin library function
myLast = last

-- Use indexing and list length
myLast xs = xs !! (length xs - 1)

-- First element in the reverse list
myLast = head . reverse
--}



-- 2) Find the next to the last (penultimate) item in a last item in a list

myButLast :: [a] -> a
myButLast [] = error "There is no but last element in an empty list"
myButLast [x] = error "There is no but last element in a single item list"
myButLast xs = second $ myReverse xs
   where second :: [a] -> a
         second (_:x:_) = x

{-- Other solutions:
--Using builtins library functions
myButLast = last . init

-- with indexing
myButLast xs = xs !! (length xs-2)
--}



-- 3) Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Element is beyond the end of the list"
elementAt (x:_) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)



-- 4) Find the number of elements of a list.

myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc+1) 0



-- 5) Reverse a list.

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]



-- 6) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = myReverse x == x



-- 7) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
-- We have to define a new data type, because lists in Haskell are homogeneous.

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten x = []



-- 8) Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (a:b:xs) | a == b    = compress (b:xs)
                  | otherwise = a : (compress (b:xs))



-- 9) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

pack :: (Eq a) => [a] -> [[a]]
pack = map (\(c,e) -> replicate c e) . encode


-- 10) Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode :: (Eq a) => [a] -> [(Int,a)]
encode [] = []
encode (x:xs) = encode' 1 x xs
  where encode' :: (Eq a) => Int -> a -> [a] -> [(Int,a)]
        encode' c e [] = [(c,e)]
        encode' c e (x:xs) | x == e    = encode' (c+1) e xs
                           | otherwise = (c,e) : (encode' 1 x xs) 


-- 11)