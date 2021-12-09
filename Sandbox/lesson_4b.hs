import Data.Char(toUpper)
-- Lazy evaluation
expensive :: Integer -> Integer
expensive n
  | n <= 1    = 1
  | otherwise = expensive (n-1) + expensive (n-2)

choice :: Bool -> a -> a -> a
choice False x y  = x
choice True x y   = y



-- Stack overflow
sum0 []     = 0
sum0 (n:ns) = n + sum0 ns
-- sum0 [0..100000000] gives stack overflow

-- tail recursive
sum1 = s 0
  where 
    s acc []      = acc
    s acc (n:ns)  = s (n+acc) ns

-- The tail recursive pattern: foldl
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v []     = v
foldl' f v (x:xs) = foldl f (f v x) xs


-- Force things to evaluate
-- seq :: a -> b -> b
-- ($!) :: (a -> b) -> a -> b
-- f $! x = x `seq` f x

sum2 = s 0
  where 
    s acc []      = acc
    s acc (n:ns)  = acc `seq` s (n+acc) ns


-- One last example
shoutify :: FilePath -> IO String
shoutify f = do
  contents <- readFile f
  let shout = map toUpper contents
  last shout `seq` writeFile f shout
  return shout