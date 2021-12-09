{- Lab 2
   Date: 21-11-09
   Authors: Jibbril Ndaw Berbres and Valdemar Stenhammar
   Lab group: 32
 -}

module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

-- | Dummy data for testing
hand2 = Add (Card Ace Hearts) (Add (Card Ace Spades) Empty)
testCard = Card Jack Clubs
h1 = hand2
h2 = Add (Card Queen Diamonds) h1
h3 = Add (Card Ace Hearts) h2
h4 = Add (Card (Numeric 7) Clubs) h3
h5 = Add (Card (Numeric 5) Clubs) h4
t1 = Add (Card (Numeric 2) Spades) (Add (Card (Numeric 1) Spades) Empty)
t2 = Add (Card (Numeric 4) Spades) (Add (Card (Numeric 3) Spades) Empty)
t3 = Add (Card (Numeric 6) Spades) (Add (Card (Numeric 5) Spades) Empty)

-- =================== A0 ===================
sizeSteps :: [Integer]
sizeSteps = [ 
    size hand2, 
    size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)),
    1 + size (Add (Card Jack Spades) Empty),
    1 + (1 + size Empty),
    1 + (1 + 0),
    1 + 1,
    2
  ]

-- =================== A1 ===================
-- | Shows a Hand of Cards, utility function to get display
-- | whitelines correctly when playing the game
display :: Hand -> String
display Empty     = ""
display h = "\n" ++ showHand h 

-- | Shows a Hand of Cards
showHand :: Hand -> String
showHand Empty     = ""
showHand (Add c h) = showCard c ++ "\n" ++ showHand h

-- | Shows a Card
showCard :: Card -> String
showCard (Card (Numeric n) s)  = show n ++ " of " ++ show s
showCard c                     = show (rank c) ++ " of " ++ show (suit c) 


-- =================== A2 ===================
-- value :: Hand -> Integer

-- | Calculates the value of a Hand
value :: Hand -> Integer
value Empty   = 0
value h 
  | val > 21  = initialValue h 1
  | otherwise = val
  where 
    val = initialValue h 11

    initialValue :: Hand -> Integer -> Integer
    initialValue Empty _ = 0
    initialValue (Add c h) a = valFromRank (rank c) a + initialValue h a

    valFromRank :: Rank -> Integer -> Integer
    valFromRank r a =
      case r of
        Ace         -> a
        (Numeric n) -> n
        _           -> 10


-- =================== A3 ===================
-- | Checks to see whether a player is bust
gameOver :: Hand -> Bool
gameOver Empty  = False
gameOver h      = value h > 21


-- =================== A4 ===================
-- | Checks to see who won the game
-- | First entry is the player, second is the bank
winner :: Hand -> Hand -> Player
winner h1 h2 
  | vH1 > vH2 && vH1 <= 21  = Guest
  | vH1 <= 21 && vH2 > 21   = Guest
  | otherwise               = Bank
    where 
      vH1 = value h1
      vH2 = value h2

-- ====================================================================
-- ============================== LAB 2B ==============================
-- ====================================================================

-- =================== B1 ===================
-- | Puts one Hand on top of another
-- | The second Hand is put on top of the first
(<+) :: Hand -> Hand -> Hand
(<+) h1 Empty = h1
(<+) h1 (Add c h) = Add c (h1 <+ h)

-- | Tests the associativity of the <+ operator
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

-- | Tests size consistency of the <+ operator
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == size h1 + size h2


-- =================== B2 ===================
-- | Generate a Hand consisting of a full deck of cards
fullDeck :: Hand
fullDeck = createHand deckList Empty
  where
    -- | Generate all Ranks
    ranks :: [Rank]
    ranks = [Numeric n | n <- [2..10]] ++ [Jack, Queen, King, Ace]

    -- | All Ranks
    suits :: [Suit]
    suits = [Hearts, Spades, Diamonds, Clubs]

    -- | Generate all combinations (full deck) in a list
    deckList :: [Card]
    deckList = [Card r s | s <- suits, r <- ranks]

    -- | Convert list to Hand
    createHand :: [Card] -> Hand -> Hand
    createHand [] h     = h
    createHand (x:xs) h = Add x (createHand xs h)

-- =================== B3 ===================
-- | Draw a Card from a Hand and add to another 
-- | The first Hand is the deck, second is the player drawing
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add c deck) hand = (deck, Add c hand) 

-- =================== B4 ===================
-- | Plays a round for the bank using a provided hand
-- | and the fullDeck command
playBank :: Hand -> Hand
playBank h = snd $ playBankHelper h Empty
  where
    -- | Enforces the logic provided in the assignment (computer
    -- | draws until Hand has value 16 or higher)
    playBankHelper :: Hand -> Hand -> (Hand,Hand)
    playBankHelper deck hand 
      | value hand < 16 = playBankHelper smallerDeck biggerHand
      | otherwise       = (deck,hand)
      where (smallerDeck,biggerHand) = draw deck hand

-- =================== B5 ===================

-- Note to reviewer: We have had a hard time combining the
-- removeNth and getNth functions into one. Ideally we would 
-- be able to both retrieve and remove the card in one function
-- call (removeAndGet :: Hand -> Integer -> (Card, Hand) ). 
-- If you have some technique for accomplishing this it
-- would be much appreciated if you could show it in your 
-- comments.

-- | Remove the n:th Card from a Hand
removeNth :: Hand -> Integer -> Hand
removeNth (Add c h) n 
  | h == Empty  = Empty
  | n == 0      = h
  | otherwise   = Add c (removeNth h (n-1))

-- | Get the n:th Card from a Hand
getNth :: Hand -> Integer -> Card
getNth (Add c h) n 
  | n == 0      = c
  | otherwise   = getNth h (n-1)

-- | Shuffle a Hand
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck stg h = shuffleHelper stg h Empty
  where
    shuffleHelper :: StdGen -> Hand -> Hand -> Hand
    shuffleHelper stg h acc 
      | h == Empty  = acc
      | otherwise   = shuffleHelper newStg smallerDeck acc'
      where
        (index,newStg) = randomR (0,size h-1) stg
        smallerDeck = removeNth h index
        acc' = Add (getNth h index) acc

-- | Tests that a Card in the original Hand remains in the
-- | shuffled Hand
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- | Helper function 
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- | 
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle stg h = size h == size (shuffleDeck stg h)


-- =================== B6 ===================
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation