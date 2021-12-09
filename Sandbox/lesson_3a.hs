import Data.List
import Data.Char
import System.Random


-- command writes text to new file 
-- writeFile "ex1.txt" "Hello World\n"

-- command writes reads from file 
-- readFile "ex1.txt"


-- Introductory example
copyFile :: FilePath -> FilePath -> IO()
copyFile fromFile toFile = do 
  c <- readFile fromFile
  writeFile toFile c


-- Find longest string in file of words
longest :: IO String
longest = do
  wlist <- readFile "words.txt"
  return (long wlist)
 
  where 
    -- Good practice to separate pure functions from IO
    long :: String -> String
    long = snd . maximum . map (\w -> (length w,w)) . words 


-- dotwice
dotwice :: IO a -> IO (a,a)
dotwice i = do
  a1 <- i
  a2 <- i
  return (a1,a2)

-- dont
dont :: IO a -> IO ()
dont i = return ()

-- return function doesn't work like in imperative languages
test :: IO Int
test = do
  return 42
  return 0   -- returns this

-- sequence_
mySequence_ :: [IO a] -> IO ()
mySequence_ []      = return ()
mySequence_ (i:is)  = do
  i
  mySequence_ is

mySequence_' (i:is)  = i >> mySequence_' is -- equivalent

-- sequence
mySequence :: [IO a] -> IO [a]
mySequence [] = return []
mySequence (i:is) = do
  a <- i
  as <- mySequence is
  return (a:as)


-- exercises
copyAll :: [FilePath] -> FilePath -> IO()
copyAll [] _ = return ()
copyAll (x:xs) toFile = do
  appendFile toFile x
  copyAll xs toFile

forLoop :: [a] -> (a -> IO ()) -> IO ()
forLoop [] _      = return ()
forLoop (x:xs) f  = do
  f x
  forLoop xs f


-- ================= Hangman game =================
wordFile = "words.txt"
guessLimit = 10

main :: IO()
main = do
  w <- randomWord
  gameLoop w ""

randomWord :: IO String
randomWord = do
  wlist <- readFile wordFile
  let ws = words wlist
  n <- randomRIO (1,length ws - 1)
  return (ws !! n)

gameLoop :: String -> String -> IO()
gameLoop w g 
  | win       = showWin
  | lose      = showLose
  | otherwise = do 
      displayStatus
      guesses <- getLine
      gameLoop w (g `union` take lives guesses)
  where 
    win           = and [ c `elem` g | c <- w] -- all (`elem` g) w
    lives         = guessLimit - length (g \\ w)
    lose          = lives <= 0
    showWin       = putStrLn $ "Win! " ++ w
    showLose      = putStrLn $ "Lose! " ++ w
    displayStatus = do
      putStrLn [ if c `elem` g then c else '_' | c <- w]
      putStrLn $ "Type your guess (" ++ show lives ++ " remaining)"

