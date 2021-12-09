import Data.Char
import Data.List(sort,group,groupBy)
-- ================== Map examples ==================
-- Def with list comprehensions
map' f xs = [f x | x <- xs]
filter' f xs = [f x | x <- xs, f x]


-- ================== foldr ==================
-- sum' [] = 0
-- sum' (n:ns) = n + sum' ns
sum' :: Num a => [a] -> a
sum' = foldr (+) 0 

-- and' [] = True
-- and' (b:bs) = b && and' bs
and' bs = foldr (&&) True bs

-- Generalized form of examples above
-- b is basecase, the identity element for a given operation. 
-- Addition => 0 (1+0 = 1), multiplication => 1 (20*1 = 20)
foldr' op b [] = b
foldr' op b (x:xs) = x `op` foldr' op b xs

-- ================== unlines ==================
str = "lorem ipsum \nbordem pipsum\n hipsum\n"
unlines' ss = foldr joinNL "" ss
  where joinNL s1 s2 = s1 ++ "\n" ++ s2

-- ================== takeLine, takeWord  ==================
takeLine ""                 = ""
takeLine (c:cs) | c /= '\n' = c:takeLine cs
                | otherwise = ""

takeWord ""                       = ""
takeWord (c:cs) | not (isSpace c) = c:takeWord cs
                | otherwise       = ""

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x:takeWhile' p xs
                    | otherwise = []

-- ================== lambda notation vs sections,  ==================

takeLine' cs = takeWhile' (\x -> x /= '\n') cs -- lambda
takeLine'' cs = takeWhile' (/= '\n') cs -- sections


-- ================== lines,  ==================
lines' []   = []
lines' css  = takeWhile (/= '\n') css 
  : lines' (drop 1 (dropWhile (/= '\n') css))

-- ================== General form, segments  ==================
nosh = "spam,eggs,chips,spam"
commSep [] = []
commSep css = takeWhile (/= ',') css : commSep (drop 1 (dropWhile (/= ',') css ))

segments p [] = []
segments p xs = takeWhile p xs : segments p (drop 1 (dropWhile p xs))


-- ================== Partial application ==================
testfun :: Char -> Bool -> String -> String
testfun c b s = c:s ++ show (not b) ++ s


-- ================== Finishing application ==================
wordCount = unlines 
  . map (\(w,n) -> w ++ ": " ++ show n) -- Format for clarity -> "hej: 2"
  . map (\ws -> (head ws, length ws)) -- Extract first item of each list of lists together with length -> ("hej", 2)
  . group -- group equal words into arrays so ["hej", "pa", "hej"] -> [["hej", "hej"], ["pa"]]
  . sort  -- sort words 
  . words -- split string into array of individual words
