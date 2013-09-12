-- pe54
-- Poker hands
-- The file, poker.txt, contains one-thousand random hands dealt to two players.
-- Each line of the file contains ten cards (separated by a single space):
-- the first five are Player 1's cards and the last five are Player 2's cards.
-- You can assume that all hands are valid (no invalid characters or repeated cards),
-- each player's hand is in no specific order, and in each hand there is a clear winner.
-- How many hands does Player 1 win?
-- Answer: xx  (0.0 real laptop seconds; with unix time)

import ProjectEuler  -- for quicksort and unique

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Suit = Diamonds | Clubs | Hearts | Spades
            deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Card = Card Rank Suit deriving (Show) 
data Hand = Hand [Card] deriving (Show) 
data Score = HighCard [Rank] | OnePair [Rank] | TwoPair [Rank] | ThreeOfAKind [Rank] |
             Straight Rank | Flush [Rank] | FullHouse [Rank] | FourOfAKind [Rank] | StraightFlush Rank
             deriving (Eq, Ord, Show)

rank :: Card -> Rank  
rank (Card r _) = r

suit :: Card -> Suit  
suit (Card _ s) = s

instance Eq Hand where  
    h1 == h2 = (score h1) == (score h2)
    
instance Ord Hand where  
    compare h1 h2 = compare (score h1) (score h2)

score :: Hand -> Score
score (Hand xs) = HighCard (rankList (Hand xs))
--FIXME FInish

isStraight :: Hand -> Bool
isStraight h = isMonotonic (rankList h) where
   isMonotonic [r] = True
   isMonotonic (r1:r2:rs) = (r1 == succ r2) && (isMonotonic (r2:rs))

isFlush :: Hand -> Bool
isFlush (Hand (c:cs)) = all $ map (\x -> (suit x) == (suit c)) cs

  

rankList :: Hand -> [Rank]
rankList (Hand xs) = reverse $ unique $ quicksort $ map rank xs

player1Score :: [(Hand,Hand)] -> Int
player1Score xs = sum [1 | (h1,h2) <- xs, h1 > h2]

makeHands :: String -> [(Hand,Hand)]
makeHands s = [(h1,h2) | line <- lines s, let cards = words line, 
                                          let h1 = makeHand (take 5 cards),
                                          let h2 = makeHand (drop 5 cards)]

makeHand :: [String] -> Hand
makeHand ss = Hand [(Card r s) | [cr,cs] <- ss, let r = readRank cr, let s = readSuit cs]

readRank :: Char -> Rank
readRank '2' = Two
readRank '3' = Three
readRank '4' = Four
readRank '5' = Five
readRank '6' = Six
readRank '7' = Seven
readRank '8' = Eight
readRank '9' = Nine
readRank 'T' = Ten
readRank 'J' = Jack
readRank 'Q' = Queen
readRank 'K' = King
readRank 'A' = Ace

readSuit :: Char -> Suit
readSuit 'S' = Spades
readSuit 'H' = Hearts
readSuit 'C' = Clubs
readSuit 'D' = Diamonds

main = do
  input <- getContents
  print $ player1Score (makeHands input)
