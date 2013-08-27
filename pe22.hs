--  pe22
--  Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by
-- sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its
-- alphabetical -- position in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name -- in the list. So, COLIN would obtain a score of 938  53 = 49714.

-- What is the total of all the name scores in the file?
-- Answer: 871198282  (0.38sec laptop time)

quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted

nameValue :: String -> Int -> Int
nameValue s n = scoreName s * n
    where scoreName = sum . map charValue
          charValue c = fromEnum c - 64

score :: [String] -> Int
score ss = sum $ zipWith nameValue (quicksort ss) [1..]

makeList :: String -> [String]
makeList s = read ("[" ++ s ++ "]")  :: [String]

main = do
  names <- getContents
  print $ score $ makeList names
