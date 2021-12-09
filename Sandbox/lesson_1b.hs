
data Colour = Red | Black
  deriving (Show,Eq)

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show,Eq)

data Rank = Numeric Int |Jack | Queen | King | Ace
  deriving (Show,Eq,Ord)

prop_Rank (Numeric n) = n > 1 && n <= 10
prop_Rank _           = True

colour :: Suit -> Colour
colour Spades = Black
colour Clubs  = Black
colour _      = Red -- Anything that's not black
-- colour Hearts   = Red
-- colour Diamonds = Red

colourCard :: Card -> Colour
colourCard c = colour (suit c)

-- Alternative way of defining
colour' :: Suit -> Colour
colour' suit =
  case suit of
    Spades  -> Black
    Clubs   -> Black
    _       -> Red

rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1 > r2

prop_rankBeats r1 r2 = rankBeats r1 r2 || r1 == r2 || rankBeats r2 r1


-- Definition of Cards
data Card = Card Rank Suit
  deriving (Show,Eq)

suit :: Card -> Suit
suit (Card _ s) = s

rank :: Card -> Rank
rank (Card r _) = r

-- Equivalent
data Card' = Card' {rank'::Rank, suit'::Suit}
  deriving (Show,Eq)


-- Card methods
cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = rankBeats r1 r2 && s1 == s2

-- Equivalent
card1 `cardBeats'` card2 = suit card1 == suit card2 && (rank card1 `rankBeats` rank card2)


-- Hand of cards
david = Card King Hearts
death = Card Ace Spades

-- type Hand = [Card] -- Alias
data Hand = Empty | Add Card Hand
  deriving Show

ex1 = Add david Empty
ex2 = Add death ex1

size :: Hand -> Int
size Empty = 0
size (Add c h) = 1 + size h


-- handBeats function
handBeats :: Hand -> Card -> Bool
handBeats Empty c       = False
handBeats (Add c' h)  c = c' `cardBeats` c || h `handBeats` c
-- handBeats (Add c' h)  c = cardBeats c' c || handBeats h c



aHand = Add death (Add david Empty)

splitHand :: Hand -> (Hand,Hand)
-- Three ways of doing the same thing

-- First version
splitHand h = (select Red h, select Black h)
select :: Colour -> Hand -> Hand
select col Empty = Empty 
select col (Add c h)  | col == colourCard c = Add c (select col h)
                      | otherwise           = select col h

-- Second version
splitHand' Empty = (Empty, Empty)
splitHand' (Add c h) | colourCard c == Red = (Add c reds, blacks)
                     | otherwise           = (reds, Add c blacks)
  where (reds, blacks) = splitHand' h

-- Third version
splitHand'' h = split h Empty Empty
  where split Empty reds blacks = (reds,blacks)
        split (Add c h) reds blacks
          | colourCard c == Red = split h (Add c reds) blacks
          | otherwise           = split h reds (Add c blacks)



