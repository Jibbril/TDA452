import Test.QuickCheck
import Data.List

-- Reminder about IO
getName :: IO String
getName = do
  putStr "Please type your name: "
  getLine

-- doTwice :: IO a -> IO (a, a) --- This is what it uses for the implementation here
-- doTwice :: Monad m => m b -> m (b, b) --- This is what haskell gives it automatically
doTwice io = do
  a <- io
  b <- io
  return (a,b)

-- Generate natural numbers
nats :: Gen Integer
nats = do 
  n <- arbitrary
  return (abs n)

-- Generate even numbers
evens :: Gen Integer
evens = do 
  n <- arbitrary
  return (2*n)

-- equivalent
evens' :: Gen Integer
evens' = (2*) <$> arbitrary

-- useful commands
-- sample (arbitrary :: Gen Card)                   --- Sample any data type
-- sample $ listOf nats                             --- Generates lists of nats
-- sample $ vectorOf 3 nats                         --- Generates vectors of length 3 of nats
-- sample $ choose (1,10)                           --- Generates values in the provided range
-- sample $ listOf1 $ choose ('a','z')              --- Generates strings
-- sample $ oneof [return 42, choose (-1,1), evens] --- Selects one out of many generators
-- sample $ elements ["Yes", "No"]                  --- Selects one of the provided elements


-- Examples on lab 2
data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show,Eq,Enum)

instance Arbitrary Suit where
  arbitrary = elements [Spades .. Clubs]


data Rank = Numeric Int | Jack | Queen | King | Ace
  deriving (Eq,Show,Ord)

rNumeric, rRoyal :: Gen Rank
rNumeric = do 
  n <- choose (2,10)
  return (Numeric n) -- Numeric <$> choose (2,10)

rRoyal = elements [Jack, Queen, King, Ace]

instance Arbitrary Rank where
  arbitrary = frequency [(9,rNumeric), (4,rRoyal)]

prop_rank (Numeric n) = n <= 10 && n>1
prop_rank _           = True

prop_rank' r = classify (r < Jack) "Numeric" $ prop_rank r


data Card = Card Suit Rank
  deriving (Eq,Show)

instance Arbitrary Card where
  arbitrary = do
    s <- arbitrary
    r <- arbitrary
    -- (s,r) <- arbitrary
    return $ Card s r


data Hand = Empty | Add Card Hand
  deriving Show

instance Arbitrary Hand where
  arbitrary = do
    cards <- arbitrary
    return $ toHand (nub cards)


toHand :: [Card] -> Hand
toHand = foldr Add Empty


-- How to use different generator than arbitrary? Use forAll
newtype = Poker = Poker Hand -- newtype only usable for new data type with only one argument
  deriving Show

instance Arbitrary Poker where
  arbitrary = do
    cs <- vectorOf 5 arbitrary
    return $ Poker (toHand cs)

-- prop_poker (Poker h) = ...  --- Will now use the above arbitrary generator in stead of the default Hand one.