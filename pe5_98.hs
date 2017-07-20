-- pe98
-- Anagram Squares:
-- Answer: 18769 137^2 = BROAD = BOARD = 17689 = 133^2 unix time real	0m1.202s
-- Basic idea, put words in order of their length, and the sorted list of their characters.  matches will be adjacent
-- with the same length, and the same list of sorted characters.
-- do something similar with the numbers which when squared will be in the range 1023... to 987... where the number of digits
-- matches the number of characters.  fortunately, there were no anagrams with words over nine characters.
-- Also, there were no anagrams with double letters.
-- filter out the numbers with duplicate digits.
-- start with the largest anagrams, and match them against the anagram squares to see if there reordering is the same.
-- list those, and if there are two with the same length, manually look for the largest.

import ProjectEuler (unique, quicksort, digitsToInt, digits, isqrt)

solve a = filter (\(a,b,c,d) -> match a b c d) [(w1,w2, digits i1, digits i2) | (l,s,w1,w2) <- a, nums <- anagramSquares l, i1 <- nums, i2 <- nums, i1 /= i2]

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

match :: [Char] -> [Char] -> [Int] -> [Int] -> Bool
match w1 w2 n1 n2 = and [match' n1 n2 i1 i2 | (i1,i2) <- wordIndices]
                || (and [match' n2 n1 i1 i2 | (i1,i2) <- wordIndices])
  where match' l1 l2 i1 Nothing = False
        match' l1 l2 i1 (Just i2) = l1!!i1 == l2!!i2
        wordIndices = zip [0..] (newIndicies w1 w2)
        newIndicies w1 w2 = [findKey a (zip w1 [0..]) | a <- w2]

anagramSquares n = reverse $ anagramSquares' n
anagramSquares' n = map (\x -> map (snd) x) $ filter ((1<).length) $ group $ squares n

numMatch :: (Eq a) => (a,b) -> (a,b) -> Bool
numMatch d1 d2 = (fst d1) == (fst d2)
group                   :: Eq a => [(a,b)] -> [[(a,b)]]
group                   =  groupBy numMatch
groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs

squares n = filter (\(a,b) -> (length $ unique a) == n) $ squares' n

squares' n = quicksort [(x2',x2) | x <- [xmin..xmax], let x2 = x^2, let x2' = quicksort $ digits x2]
  where xmin = 1 + (isqrt $ digitsToInt (1:0:[2..(n-1)]))
        xmax = isqrt $ digitsToInt [9,8..(10-n)]


anagrams w = let a = anagrams2x w in reverse $ quicksort (a ++ (anagrams3x a))

anagrams2x w = map merge $ filter match $ zip w ((0,"",""):w)
  where match ((l1, c1, w1),(l2, c2, w2)) = l1 == l2 && c1 == c2
        merge ((l1, c1, w1),(l2, c2, w2)) = (l1,c1,w1,w2)

-- gets any the missing pair for any triple anagrams
anagrams3x w = map merge $ filter match $ zip w ((0,"","",""):w)
  where match ((l1, c1, w1, w2),(l2, c2, w3, w4)) = l1 == l2 && c1 == c2 && w2 == w3
        merge ((l1, c1, w1, w2),(l2, c2, w3, w4)) = (l1,c1,w1,w4)

wordInfo :: [String] -> [(Int, String, String)]
wordInfo ws = reverse $ quicksort [(length w, quicksort w, w) | w <- ws]

-- make a list of words, the input is a string of double quoted words separated by commas
wordList :: String -> [String]
wordList s = read ( "[" ++ s ++ "]" ) :: [String]

main = do
  input <- getContents
  -- test list of anagrams
  --print $ anagrams $ wordInfo $ wordList input
  -- make sure there are no duplicate letters
  --print $ filter (\(a,b,_,_,_) -> a /= b) $ map (\(a,b,c,d) -> (a,length b,b,c,d)) $ anagrams $ wordInfo $ wordList input
  -- solve
  print $ take 4 $ solve $ anagrams $ wordInfo $ wordList input
