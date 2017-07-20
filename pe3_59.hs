-- 59
-- XOR decryption: the encryption key consists of three lower case characters, decrypt the message and find the sum of the ASCII values in the original text.
-- the message is in a file containing the encrypted ASCII codes; the plain text must contain common English words.
-- Answer: 107359 time 0.00
-- To test (encode or decode) a text file:
--   cat pe3_59cipher1.txt | ./pe3_59 t abc | ./pe3_59 t abc  should return the input file
-- Analysis:
--   run with ./pe3_59 a < pe3_59cipher1.txt
--   the Histogram of the 1,4,7.. (every letter XOR'd by the first char of the passcode is (count, code)
--    [(70,71),(48,2),(28,19),(28,9),(25,15),(18,14),(18,8),(18,6),(17,20),(15,21),(12,11),(12,0),(11,73),(8,3),(7,16),(6,10),(6,1),(5,75),(5,18),(5,5),(5,4),(4,32),(4,17),(3,45),(3,30),(3,23),(2,86),(2,31),(2,12),(1,92),(1,87),(1,84),(1,83),(1,79),(1,78),(1,64),(1,52),(1,41),(1,37),(1,33)]
--   similarly for the 2,5,8...
--    [(85,79),(35,7),(31,10),(28,6),(26,0),(21,27),(20,28),(20,14),(18,3),(16,11),(16,1),(9,24),(8,8),(8,2),(6,29),(6,26),(6,25),(6,9),(5,22),(5,12),(4,59),(3,13),(2,67),(2,65),(2,56),(2,39),(2,31),(1,94),(1,91),(1,89),(1,87),(1,72),(1,60),(1,45),(1,38)]
--   and 3,6,9...
--    [(77,68),(41,1),(31,16),(26,11),(24,12),(23,10),(21,13),(17,22),(16,5),(14,23),(11,3),(11,0),(8,8),(7,19),(7,9),(6,7),(5,85),(5,29),(5,20),(5,18),(5,17),(4,74),(4,72),(4,2),(3,35),(3,6),(2,86),(2,44),(1,93),(1,87),(1,83),(1,81),(1,69),(1,48),(1,40),(1,38),(1,37),(1,33),(1,30),(1,28),(1,15)]
--   from the wikipedia page on number frequencies, space is slightly higher frequency than e,t,a,o,i,n, with e at about ~13% and t at ~8%
--   therefore, the first code may be 71 xor 32 (space), 2 xor {101|69} (e,E) or  (19|9) xor {116, 84} ((t,T) = 85, looking at these permutations
--   [ a `xor` b | a <- [71,2,19,9], b <- [32,101,69,116,84]]  -> [2,19,34,34,41,51,51,71,71,76,86,86,93,103,103,103,108,118,118,125] (sorted)
--   since 103 (g) appears more than any other, it is a likely candidate
--   [ a `xor` b | a <- [79,7,10], b <- [32,101,69,116,84]]  -> [10,27,39,42,42,59,66,79,83,94,98,111,111,115,126]
--   since 111 (o) appears more than any other, it is a likely candidate
--   [ a `xor` b | a <- [68,1,16], b <- [32,101,69,116,84]]  -> [1,16,33,33,48,48,68,68,85,85,100,100,100,117,117]
--   since 100 (d) appears more than any other, it is a likely candidate
--   lo and behold, the code "god" yields (The Gospel of John, chapter 1)...
-- To see if a code creates english text from the list of ascii values given
--   ./pe3_59 s god < pe3_59cipher1.txt
-- To get the sum of the ascii values in the decoded english text from the list of ascii values given
--   ./pe3_59 f god < pe3_59cipher1.txt


import System.Environment ( getArgs )
import Data.Bits ( xor )
import ProjectEuler ( quicksort )


asciify :: String -> [Int]
asciify = map fromEnum

deasciify :: [Int] -> String
deasciify = map (\i -> toEnum i ::Char)

xorEncode :: String -> String -> String
xorEncode key text = deasciify $ zipWith (xor) (asciify text) (cycle (asciify key))

xorEncode' :: String -> [Int] -> [Int]
xorEncode' key text = zipWith (xor) text (cycle (asciify key))

intList :: String -> [Int]
intList numbers = read ('[':(numbers ++ "]"))

solution :: String -> String -> String
solution key numbers = deasciify $ xorEncode' key (intList numbers)

final :: String -> String -> Int
final key numbers = sum $ xorEncode' key (intList numbers)

partition3 :: [a] -> ([a], [a], [a])
partition3 list = partition3' ([],[],[]) list where
    partition3' (p1,p2,p3) [] = (p1,p2,p3)
    partition3' (p1,p2,p3) (x:[]) = (x:p1,p2,p3)
    partition3' (p1,p2,p3) (x:y:[]) = (x:p1,y:p2,p3)
    partition3' (p1,p2,p3) (x:y:z:r) = partition3' (x:p1,y:p2,z:p3) r

removeItem :: (Eq b) => (a,b) -> [(a,b)] -> [(a,b)]
removeItem _ []                          = []
removeItem x (y:ys) | (snd x) == (snd y) = ys
                    | otherwise          = y : removeItem x ys

histogram :: (Eq a) => [a] -> [(Int,a)]
histogram xs = foldr step [] xs where
  step ele acc = let match = [item | item <- acc, (snd item) == ele]
                 in if   (null match)
                    then ((1                   ,ele):acc)
                    else ((1+(fst (head match)),ele):(removeItem (head match) acc))


analyze numbers = let (l1,l2,l3) = (partition3 (intList numbers))
                  in map (reverse . quicksort . histogram)  [l1,l2,l3]


main = do
  args <- getArgs
  input <- getContents
  case args of ("t":key:_) -> putStr $ xorEncode key input
               ("a":_)     -> print (analyze input)
               ("s":key:_) -> putStr $ solution key input
               ("f":key:_) -> print $ final key input
