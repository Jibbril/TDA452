import ModuleTest.FirstModule
import System.Random

dieRoll :: StdGen -> (Integer,Integer)
dieRoll g = (n1, n2)
  where (n1, g1) = randomR (1,6) g
        (n2, _ ) = randomR (1,6) g1


-- Constants
val :: Integer
val = 38

-- Tuples
ex1 :: Integer -> (Bool,Integer)
ex1 n = (n < 0, abs n)

-- Lists
summary :: [String] -> String
summary []  = "Nothing"
summary [s] = "Just " ++ s 
summary [s1,s2] = s1 ++ " and " ++ s2
summary (s:rest) = s ++ " followed by a bunch of things and ending with" ++ last rest


-- Defining functions on lists
len :: [s] -> Integer
len []     = 0
len (x:xs) = len xs + 1

last' :: [p] -> p
last' []     = error "Last of empty list"
last' [a]    = a 
last' (x:xs) = last' xs

-- List Comprehensions
ten = [1..10]
comp = [n | n <- ten] -- Copy every item in array
comp2 = [n | n <- ten, odd n] -- Conditional list comprehension
comp3 = [ (n,c) | n <- [1,2,3], c <- "abc"] -- Create all combinations
comp4 = [ (n,c) | n <- [1,2,3], c <- "abc", c == 'a'] -- Conditional

-- Quicksort
qsort []     = []
qsort (n:ns) = qsort smaller ++ [n] ++ qsort bigger
  where smaller = [s | s <- ns, s <= n]
        bigger = [b | b <- ns, b > n]



