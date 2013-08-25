-- Project Euler in Haskell
-- By Regan Sarwas August 2013 to ...
-- Although some of these problems can be simplified by loading a standard or third party module,
--  I am limiting myself to just the functions available in the Prelude module loaded by GHCi.
-- It is intended that this code be run with the Haskell interpreter (GHCi)
-- 1. Download the Haskell platform from http://www.haskell.org/platform/
-- 2. Launch ghci in a terminal window
-- 3. From the ghci prompt, load this file with ':l pe'
-- 4. To get the solution to the first problem, just enter pe1 at the ghci prompt

-- 12) divisors

-- foldl  f acc [a1, a2,     ... an]  = f (... (f (f acc a1) a2) ...) an
-- foldl1 f acc [a1, a2, a3, ... an]  = f (... (f (f  a1 a2) a3) ...) an
-- foldr  f acc [a1, ...       an-1, an] = f a1 (... (f an-1 (f an  acc)) ...)
-- foldr1 f acc [a1, ... an-2, an-1, an] = f a1 (... (f an-2 (f an-1 an)) ...)
-- scan[l|r|l1|r1] are similar to the fold functions, except they return a list with the value of the accumulator at each step


-- 1
-- Find the sum of all the multiples of 3 or 5 below 1000
-- Answer: 233168  (0.01 secs, 2125904 bytes)
pe1 = sum $ filter multipleOf3or5 [1..999]
      where multipleOf3or5 x = x `mod` 3 == 0 || x `mod` 5 == 0


-- 2
-- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
-- Answer: 4613732  (0.00 secs, 1065392 bytes)
pe2 = sum $ filter even smallFibs
      where smallFibs = takeWhile (<4000000) fibs
            fibs = 1 : scanl (+) 1 fibs


-- 3
-- What is the largest prime factor of the number 600851475143
-- Answer: 6857  (9.42 secs, 4638462768 bytes)
pe3data = 600851475143
-- reasonably fast prime generator. Algorithm stolen from http://www.haskell.org/haskellwiki/Prime_numbers
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
--isqrt: integral square root, isqrt n returns the largest integer a such that a * a <= n
isqrt :: (Integral a) => a -> a
isqrt n = truncate (sqrt (fromIntegral n))
pe3 = head [x | x <- reverse (primesTo (isqrt pe3data)), pe3data `mod` x == 0]


-- 4
-- Find the largest palindrome made from the product of two 3-digit numbers
-- Answer 906609 = 913 * 993  (0.01 secs, 13973056 bytes)
pe4 = head $ filter has3DigitDivisor palindromes where
-- palindromes <= 997799 < 999 * 999 = 998001
   palindromes = [read (show x ++ (reverse $ show x)) :: Int | x <- [997,996..100]]
   has3DigitDivisor n
     | n < 100000 || 999*999 < n = False
     | otherwise                 = or [True | x <- [999,998..(n `div` 999)], n `mod` x == 0] 


-- 5
-- What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
-- Answer: 232792560  (0.01 secs, 11877128 bytes)
pe5 = foldl1 lcm [1..20]


-- 6
-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
-- Answer: 25164150  (0.00 secs, 1029080 bytes)
pe6 = abs (sumOfSquares [1..100] - squareOfSum [1..100])
      where sumOfSquares xs = sum [x*x | x <- xs]
            squareOfSum  xs = let x = sum xs in x*x


-- 7
-- What is the 10 001st prime number?
-- Answer: 104759  (0.72 secs, 357600072 bytes)
-- Answer: 104743  (1.20 laptop secs, 199001748 bytes) 
-- uses primesTo from pe3
-- 1st prime is at index 0; nth is at index n-1
pe7 = primesTo 105000 !! (10001-1)


-- 8
-- Find the greatest product of five consecutive digits in the 1000-digit number.
-- Answer: 40824  (0.01 secs, 3191720 bytes)
pe8data =
  "73167176531330624919225119674426574742355349194934" ++
  "96983520312774506326239578318016984801869478851843" ++
  "85861560789112949495459501737958331952853208805511" ++
  "12540698747158523863050715693290963295227443043557" ++
  "66896648950445244523161731856403098711121722383113" ++
  "62229893423380308135336276614282806444486645238749" ++
  "30358907296290491560440772390713810515859307960866" ++
  "70172427121883998797908792274921901699720888093776" ++
  "65727333001053367881220235421809751254540594752243" ++
  "52584907711670556013604839586446706324415722155397" ++
  "53697817977846174064955149290862569321978468622482" ++
  "83972241375657056057490261407972968652414535100474" ++
  "82166370484403199890008895243450658541227588666881" ++
  "16427171479924442928230863465674813919123162824586" ++
  "17866458359124566529476545682848912883142607690042" ++
  "24219022671055626321111109370544217506941658960408" ++
  "07198403850962455444362981230987879927244284909188" ++
  "84580156166097919133875499200524063689912560717606" ++
  "05886116467109405077541002256983155200055935729725" ++
  "71636269561882670428252483600823257530420752963450"
pe8 = maximum ([productDigits x | x <- consecutive 5 pe8data]) where
  productDigits s = product (map charToInt s)
  charToInt c = fromEnum c - 48
  consecutive n cs = [take n (drop x cs) | x <- [0..length cs - n]]



-- 9
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc.
-- Answer: [(200,375,425,31875000)]  (0.07 secs, 29369200 bytes)
pe9 = head $ [(a, b, c, a*b*c) | c <- [500,499..3], b <- [c-1,c-2..2], let a = 1000 - b - c, a*a + b*b == c*c]


-- 10
-- Find the sum of all the primes below two million
-- Answer: 142913828922  (32.47 secs, 16029848200 bytes)
-- uses primesTo from pe3
pe10 = sum $ primesTo 2000000


-- 11
-- What is the greatest product of four adjacent numbers in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
-- Answer: 70600674 = 87*97*94*89 from column 4, row 16 up to right  (0.01 secs, 6778152 bytes)
pe11data = 
  ["08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08",
   "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00",
   "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65",
   "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91",
   "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80",
   "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50",
   "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70",
   "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21",
   "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72",
   "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95",
   "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92",
   "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57",
   "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58",
   "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40",
   "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66",
   "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69",
   "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36",
   "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16",
   "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54",
   "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"]
intGrid :: [[Int]]
intGrid = [map read (words x)::[Int] | x <- pe11data]
adjacentProduct :: (Num a, Ord a) => Int -> [a] -> a
adjacentProduct n xs = maximum $ map product [take n (drop x xs) | x <- [0..length xs - n]]
transpose :: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)
horizontalMax = maximum $ map (adjacentProduct 4) intGrid
verticalMax = maximum $ map (adjacentProduct 4) (transpose intGrid)
-- for diagonals: add 1s at begining and end of rows to create a new matrix where diagonals are in columns
-- i.e [[0,2,3]    [[0,2,3,1,1,]        [[1,1,0,2,3]
--      [4,5,6]  -> [1,4,5,6,1,]   or    [1,4,5,6,1]
--      [7,8,9]]    [1,1,7,8,9,]]        [7,8,9,1,1]
-- then transpose and look for maximum adjacent products in each row
-- so long as the solution is greater than 99*99*99 ~ 1,000,000 we can be sure that a diagonal of three or less is not the max.
rightDiagonalMax = maximum $ map (adjacentProduct 4) (transpose [(replicate x 1) ++ (intGrid !! x) ++ (replicate (19-x) 1) | x <- [0..19]])
leftDiagonalMax = maximum $ map (adjacentProduct 4) (transpose [(replicate (19-x) 1) ++ (intGrid !! x) ++ (replicate x 1) | x <- [0..19]])
pe11 = maximum [horizontalMax, verticalMax, leftDiagonalMax, rightDiagonalMax]


-- 12 (slow and undone)
-- What is the value of the first triangle number to have over five hundred divisors?
-- Answer:
triangleNumbers = scanl1 (+) [1..]
triangleNumber n = n*(n+1) `div` 2
triangleNumbersFrom n = scanl (+) x [(n+1)..] where x = triangleNumber n

smallestNumberWith500Divisors = foldl1 lcm [1..500]  -- from pe5
-- = 73239622318952846593863874519042298829761338251289259046349190034596307420803713394327759819891326985268312606648408875713314013313623337094312440663659803352061415560955398316253892220738945585450197206138869521568000
primeFactors n = primeFactors' n (primesTo (isqrt n))
primeFactors' n possiblePrimes
  | n < 2               = []
  | null possiblePrimes = [n]
  | r == 0              = nextPrime:(primeFactors' q possiblePrimes)
  | otherwise           = primeFactors' n (tail possiblePrimes)
  where nextPrime = head possiblePrimes
        (q,r) = n `quotRem` nextPrime 

--pe12 = take 4 (dropWhile (<smallestNumberWith500Divisors) triangleNumbers)


-- 13
-- Work out the first ten digits of the sum of the following one-hundred 50-digit numbers
-- Answer: [5,5,3,7,3,7,6,2,3,0]  (0.00 secs, 1615288 bytes)
pe13data =
  [37107287533902102798797998220837590246510135740250,
   46376937677490009712648124896970078050417018260538,
   74324986199524741059474233309513058123726617309629,
   91942213363574161572522430563301811072406154908250,
   23067588207539346171171980310421047513778063246676,
   89261670696623633820136378418383684178734361726757,
   28112879812849979408065481931592621691275889832738,
   44274228917432520321923589422876796487670272189318,
   47451445736001306439091167216856844588711603153276,
   70386486105843025439939619828917593665686757934951,
   62176457141856560629502157223196586755079324193331,
   64906352462741904929101432445813822663347944758178,
   92575867718337217661963751590579239728245598838407,
   58203565325359399008402633568948830189458628227828,
   80181199384826282014278194139940567587151170094390,
   35398664372827112653829987240784473053190104293586,
   86515506006295864861532075273371959191420517255829,
   71693888707715466499115593487603532921714970056938,
   54370070576826684624621495650076471787294438377604,
   53282654108756828443191190634694037855217779295145,
   36123272525000296071075082563815656710885258350721,
   45876576172410976447339110607218265236877223636045,
   17423706905851860660448207621209813287860733969412,
   81142660418086830619328460811191061556940512689692,
   51934325451728388641918047049293215058642563049483,
   62467221648435076201727918039944693004732956340691,
   15732444386908125794514089057706229429197107928209,
   55037687525678773091862540744969844508330393682126,
   18336384825330154686196124348767681297534375946515,
   80386287592878490201521685554828717201219257766954,
   78182833757993103614740356856449095527097864797581,
   16726320100436897842553539920931837441497806860984,
   48403098129077791799088218795327364475675590848030,
   87086987551392711854517078544161852424320693150332,
   59959406895756536782107074926966537676326235447210,
   69793950679652694742597709739166693763042633987085,
   41052684708299085211399427365734116182760315001271,
   65378607361501080857009149939512557028198746004375,
   35829035317434717326932123578154982629742552737307,
   94953759765105305946966067683156574377167401875275,
   88902802571733229619176668713819931811048770190271,
   25267680276078003013678680992525463401061632866526,
   36270218540497705585629946580636237993140746255962,
   24074486908231174977792365466257246923322810917141,
   91430288197103288597806669760892938638285025333403,
   34413065578016127815921815005561868836468420090470,
   23053081172816430487623791969842487255036638784583,
   11487696932154902810424020138335124462181441773470,
   63783299490636259666498587618221225225512486764533,
   67720186971698544312419572409913959008952310058822,
   95548255300263520781532296796249481641953868218774,
   76085327132285723110424803456124867697064507995236,
   37774242535411291684276865538926205024910326572967,
   23701913275725675285653248258265463092207058596522,
   29798860272258331913126375147341994889534765745501,
   18495701454879288984856827726077713721403798879715,
   38298203783031473527721580348144513491373226651381,
   34829543829199918180278916522431027392251122869539,
   40957953066405232632538044100059654939159879593635,
   29746152185502371307642255121183693803580388584903,
   41698116222072977186158236678424689157993532961922,
   62467957194401269043877107275048102390895523597457,
   23189706772547915061505504953922979530901129967519,
   86188088225875314529584099251203829009407770775672,
   11306739708304724483816533873502340845647058077308,
   82959174767140363198008187129011875491310547126581,
   97623331044818386269515456334926366572897563400500,
   42846280183517070527831839425882145521227251250327,
   55121603546981200581762165212827652751691296897789,
   32238195734329339946437501907836945765883352399886,
   75506164965184775180738168837861091527357929701337,
   62177842752192623401942399639168044983993173312731,
   32924185707147349566916674687634660915035914677504,
   99518671430235219628894890102423325116913619626622,
   73267460800591547471830798392868535206946944540724,
   76841822524674417161514036427982273348055556214818,
   97142617910342598647204516893989422179826088076852,
   87783646182799346313767754307809363333018982642090,
   10848802521674670883215120185883543223812876952786,
   71329612474782464538636993009049310363619763878039,
   62184073572399794223406235393808339651327408011116,
   66627891981488087797941876876144230030984490851411,
   60661826293682836764744779239180335110989069790714,
   85786944089552990653640447425576083659976645795096,
   66024396409905389607120198219976047599490197230297,
   64913982680032973156037120041377903785566085089252,
   16730939319872750275468906903707539413042652315011,
   94809377245048795150954100921645863754710598436791,
   78639167021187492431995700641917969777599028300699,
   15368713711936614952811305876380278410754449733078,
   40789923115535562561142322423255033685442488917353,
   44889911501440648020369068063960672322193204149535,
   41503128880339536053299340368006977710650566631954,
   81234880673210146739058568557934581403627822703280,
   82616570773948327592232845941706525094512325230608,
   22918802058777319719839450180888072429661980811197,
   77158542502016545090413245809786882778948721859617,
   72107838435069186155435662884062257473692284509516,
   20849603980134001723930671666823555245252804609722,
   53503534226472524250874054075591789781264330331690]
digits :: Integral a => a -> [a]
digits x
     | x < 0     = digits (-x)
     | x < 10    = [x]
     | otherwise = digits (x `div` 10) ++ [(x `mod` 10)]
pe13 = take 10 $ digits $ sum pe13data


-- 14
-- Which starting number, under one million, produces the longest Collatz sequence chain?
-- Answer: (837799,525)  (347.67 secs, 97453747000 bytes)  FIXME improve time and space
collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
    | even n    = n:collatzChain (n `div` 2)
    | otherwise = n:collatzChain (3*n+1)
max' [x] = x
max' (x:xs)
  | (snd x) > (snd maxtail) = x
  | otherwise               = maxtail
  where maxtail = max' xs
pe14 = max' [(x,length $ collatzChain x) | x <- [1..999999]]


-- 15
-- How many such lattice paths are there through a 20×20 grid?
-- Answer: 137846528820  (0.01 secs, 11881816 bytes)
-- Rotate the lattice so the starting node is at the top, count the number of paths into each node
--       1
--     1   1
--   1   2   1
-- by summing the two nodes above it you will see that the values of the nodes of the lattice is the central
-- portion of pascal's triangle. all we need is the middle value of the last row. 
pe15data = 20
pascalTriangleRow 1 = [1]
pascalTriangleRow n = zipWith (+) (0:previousRow) (previousRow++[0])
  where previousRow = pascalTriangleRow (n-1)
pe15 = pascalTriangleRow (pe15data * 2 + 1) !! pe15data


-- 16
-- What is the sum of the digits of the number 2^1000?
-- Answer = 1366  (0.00 secs, 4217344 bytes)
-- uses digits from pe13
pe16 = sum $ digits $ 2^1000


-- 17
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
-- Answer: 21124  (0.15 secs, 42520248 bytes)
intWords :: Int -> String
intWords x
        | x  < 0  = "negative " ++ intWords (-x)
        | x == 0  = "zero"
        | x == 1  = "one"
        | x == 2  = "two"
        | x == 3  = "three"
        | x == 4  = "four"
        | x == 5  = "five"
        | x == 6  = "six"
        | x == 7  = "seven"
        | x == 8  = "eight"
        | x == 9  = "nine"
        | x == 10 = "ten"
        | x == 11 = "eleven"
        | x == 12 = "twelve"
        | x == 13 = "thirteen"
        | x == 15 = "fifteen"
        | x == 18 = "eighteen"
        | x  < 20 = intWords (x `mod` 10) ++ "teen"
        | x  < 30 = "twenty" ++ lastDigit x
        | x  < 40 = "thirty" ++ lastDigit x
        | x  < 50 = "forty" ++ lastDigit x
        | x  < 60 = "fifty" ++ lastDigit x
        | x  < 70 = "sixty" ++ lastDigit x
        | x  < 80 = "seventy" ++ lastDigit x
        | x  < 90 = "eighty" ++ lastDigit x
        | x < 100 = "ninety" ++ lastDigit x
        | x < 1000 = intWords (x `div` 100) ++ " hundred" ++ if (x `mod` 100) == 0 then "" else " and " ++ intWords (x `mod` 100)
        | x < 1000000 = bigNumber 3 "thousand" x
        | x < 1000000000 = bigNumber 6 "million" x
        | x < 1000000000000 = bigNumber 9 "billion" x
        where lastDigit x = if x `mod` 10 == 0 then "" else "-" ++ intWords (x `mod` 10)
              bigNumber n s x = intWords (x `div` 10^n) ++ " " ++ s ++ lastNdigits n x
              lastNdigits n x =  if x `mod` 1000 == 0 then "" else ((if x `mod` 1000 < 100 then " and " else " ") ++ intWords (x `mod` 10^n))
replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new haystack
  | haystack == old = new
  | take l haystack == old = new ++ replace old new (drop l haystack)
  | otherwise = head haystack : replace old new (tail haystack)
  where l = length old
intWordLength :: Int -> Int
intWordLength n = length (replace " and " "" (replace " " "" (replace "-" "" (intWords n))))
pe17 = sum $ map intWordLength [1..1000]  


-- 18
-- Find the maximum sum of adjacent numbers from top to bottom of the triangle below:
-- Answer: 1074 (0.00 secs, 1027952 bytes)
pe18data = 
  [[75],
   [95, 64],
   [17, 47, 82],
   [18, 35, 87, 10],
   [20, 04, 82, 47, 65],
   [19, 01, 23, 75, 03, 34],
   [88, 02, 77, 73, 07, 63, 67],
   [99, 65, 04, 28, 06, 16, 70, 92],
   [41, 41, 26, 56, 83, 40, 80, 70, 33],
   [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
   [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
   [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
   [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
   [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
   [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]
zip3With :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zip3With _ [] _ _ = []
zip3With _ _ [] _ = []
zip3With _ _ _ [] = []
zip3With f (x:xs) (y:ys) (z:zs) = (f x y z) : (zip3With f xs ys zs)
maxTriSum t = head $ foldr1 sumAdjacent t 
  where sumAdjacent x y = zip3With maxSum x y (tail y)
        maxSum x y z
               | y < z     = x + z
               | otherwise = x + y
pe18 = maxTriSum pe18data


-- 19
-- Given: 1 Jan 1900 was a Monday, How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-- Answer:  171 (0.33 secs, 106301200 bytes)
-- Dec 31 1899 was a Sunday, let that be day 0, then 1 Jan 1900 = day 1, therefore day `mod` 7 == 0 means Sunday
pe19 = sum [1 | y <- [1901..2000], d <- [1..daysInYear y], isFirstDayOfMonth y d && isSunday y d]
     where daysInYear y = if isLeapYear y then 366 else 365
           isFirstDayOfMonth y d = d `elem` (if isLeapYear y then firstDayOfMonth' else firstDayOfMonth)
           isSunday y d = (d + (y - 1900) * 365 + leapDaysBetween 1900 y) `mod` 7 == 0
           leapDaysBetween y1 y2 = sum [1 | y <- [y1..(y2-1)], isLeapYear y]
           isLeapYear y = (y `mod` 4 == 0) && ((y `mod` 100 /= 0) || (y `mod` 400 == 0))
           firstDayOfMonth  = scanl (+) 1 [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30]
           firstDayOfMonth' = scanl (+) 1 [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30]


-- 20
-- Find the sum of the digits in the number 100!
-- Answer: 648  (0.00 secs, 2129736 bytes)
-- uses digits from pe16
pe20 = sum $ digits $ product [1..100]



            