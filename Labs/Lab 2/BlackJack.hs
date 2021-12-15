{- 
** Jibbril Ndaw Berbres
** Valdemar Stenhammar
** Group 32
-}

module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import Data.Text.Internal
import System.Random

-- | Assignment A0: sizeSteps shows how "size hand2" is evaluated.
hand2 = (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))

sizeSteps :: [Integer]
sizeSteps = [size hand2
            , size hand2
            , size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 2 + 0
            , 2]



-- | Assignment A1: Two methods that turns the Cards in a Hand to String.

-- | Displays a hand of cards.
display :: Hand -> String
display Empty            = ". \n \n"
display (Add card Empty) = displayCard card ++ display Empty
display (Add card hand)  = displayCard card ++ ", \n" ++ display hand

-- | Displays a card.
displayCard :: Card -> String
displayCard (Card (Numeric r) s) = show r ++ " of " ++ show s
displayCard (Card r s)           = show r ++ " of " ++ show s


-- | Assignment A2:

-- | Calculates the final value of a Hand.
value :: Hand -> Integer
value Empty = 0
value hand | val > 21  = val - numberOfAces hand * 10
           | otherwise = val
        where val = initialValue hand

-- | Calculates total number of aces in a Hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | otherwise          =     numberOfAces hand

-- | Calculates value of Hand, disregarding number of aces.
initialValue :: Hand -> Integer
initialValue Empty           = 0
initialValue (Add card hand) = valueRank (rank card) + initialValue hand
        where 
           valueRank (Numeric r)               = r
           valueRank rank        | rank == Ace = 11
                                 | otherwise   = 10


-- | Assignment A3:

-- | Checks if a Hand is over the 21-limit.
gameOver :: Hand -> Bool
gameOver hand | value hand > 21 = True
              | otherwise       = False


-- | Assignment A4:

-- | Checks if Guest or Bank won. Guest is first argument, Bank second.
winner :: Hand -> Hand -> Player
winner h1 h2 
  | (bank >= guest && bank <= 21) || guest > 21 = Bank
  | otherwise                                 = Guest
     where guest = value h1
           bank  = value h2


-- | Assignment B1:

-- Combines two hands by adding the second one on top of the first.
(<+) :: Hand -> Hand -> Hand
Empty       <+ hand = hand
(Add c1 h1) <+ hand = Add c1 (h1 <+ hand)


-- | Assignment B2:

-- Creates and returns a full standard deck.
fullDeck :: Hand
fullDeck = fullSuit Hearts <+ fullSuit Spades <+ fullSuit Diamonds <+ fullSuit Clubs
        where 
        ranks = Ace:King:Queen:Jack:[ Numeric n | n <- [10,9..2] ]

        fullSuit :: Suit -> Hand
        fullSuit suit = allRanks ranks suit

        allRanks :: [Rank] -> Suit -> Hand
        allRanks []  _       = Empty
        allRanks (r:rs) suit = Add (Card r suit) (allRanks rs suit)


-- | Assignment B3:

-- Adds a card from a deck to a hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add c deck) hand = (deck, (Add c hand))


-- | Assignment B4:

-- Generates a hand for the Bank.
playBank :: Hand -> Hand
playBank deck = snd $ playBankHelper deck Empty
 where
 playBankHelper :: Hand -> Hand -> (Hand, Hand)
 playBankHelper deck hand | value hand >= 16 = (deck, hand)
                          | otherwise  = playBankHelper smallerDeck biggerHand
  where (smallerDeck,biggerHand) = draw deck hand


-- | Assignment B5:

-- Shuffles the deck
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g Empty = Empty
shuffleDeck g d     = Add card (shuffleDeck gen deck)
 where (card, deck) = removeCard d index
       (index,gen)  = randomR (0, (size d)-1) g

-- Removed a card from a given index.
removeCard :: Hand -> Int -> (Card,Hand)
removeCard (Add card deck) 0     = (card,deck)
removeCard (Add card deck) index = ((fst removed), (Add card (snd removed)))
 where removed = removeCard deck (index-1)

-- | Assignment B6:

implementation = Interface
 { iFullDeck = fullDeck
 , iValue = value
 , iDisplay = display
 , iGameOver = gameOver
 , iWinner = winner
 , iDraw = draw
 , iPlayBank = playBank
 , iShuffle = shuffleDeck
 }

-- Starts the program.
main :: IO ()
main = runGame implementation


-- | Properties for testing.

-- These two belongs to assignment B1
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
 p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = 
 (size p1 + size p2) == size (p1 <+ p2)

-- These two belongs to assignment B5
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = 
        size h == (size $ shuffleDeck g h)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
 c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- Helper function for prop_shuffle_sameCards
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
