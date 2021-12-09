module L2 where

import Test.QuickCheck
import Data.List(sort)
import Prelude hiding ((++), reverse, drop, take)
import qualified Prelude as P((++), reverse, drop, take)
-- List examples from the prelude
-- append
(++) :: [a] -> [a] -> [a]
[] ++ ys      = ys
(x:xs) ++ ys  = x:(xs ++ ys)

-- reverse
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]  -- O(n^2) complexity
reverse' (x:xs) = revInto [] (x:xs)     -- O(n) complexity
  where revInto acc []      = acc 
        revInto acc (x:xs)  = revInto (x:acc) xs

-- take/drop
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x:take (n-1) xs

drop _ []         = []
drop n (x:xs) | n > 0     = drop (n-1) xs    
              | otherwise = (x:xs)

-- Simple property of take
-- ==> is quickcheck syntax, removes test values that do not meet the provided condition
-- prop_take n xs = n >= 0 ==> length (take n xs) <= n
prop_take :: Int -> [Int] -> Bool
prop_take n xs = let n' = abs n in length (take n' xs) <= n'

prop_takeDrop :: Int -> [Int] -> Bool -- Very important to pick types for properties!!!
prop_takeDrop n xs = take n xs ++ drop n xs == xs
