module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe(fromJust,mapMaybe,listToMaybe,catMaybes, isJust, isNothing)

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

badDigitExample :: Sudoku
badDigitExample =
    Sudoku
      [ [j 10,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

badSizeExample :: Sudoku
badSizeExample =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      ]
  where
    n = Nothing
    j = Just


-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate boardSize genEmptyRow)

-- | Generates an empty Row
genEmptyRow :: Row
genEmptyRow = replicate boardSize Nothing

boardSize :: Int
boardSize = 9



-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = sizeOk s && digitsOk s

-- | Checks that both the Rows and columns of the Sudoku are the correct length
sizeOk :: Sudoku -> Bool
sizeOk (Sudoku rs) = hasBoardSize rs && all hasBoardSize rs

-- | Checks that a list has length equal to boardSize
hasBoardSize :: [a] -> Bool
hasBoardSize r = length r == boardSize

-- | Checks that the values of all rows lie in the accepted range
digitsOk :: Sudoku -> Bool
digitsOk (Sudoku rs) = all digitInRange (concat rs)

-- | Checks that a Maybe Int is between 1 and boardSize, or is Nothing
digitInRange :: Maybe Int -> Bool
digitInRange Nothing  = True
digitInRange i        = i >= Just 1 && i <= Just boardSize


-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled (Sudoku rs) = Nothing `notElem` concat rs
-- isFilled (Sudoku rs) = and [ and [ d /= Nothing | d <- r] | r <- rs]

fullExample :: Sudoku
fullExample =
    Sudoku
      [ 
        [j 1, j 2,  j 3 ],
        [j 4, j 5,  j 6 ],
        [j 7, j 8,  j 9 ]
      ]
  where
    n = Nothing
    j = Just

notFullExample :: Sudoku
notFullExample =
    Sudoku
      [ 
        [n,   j 6,  j 2   ],
        [j 2, j 5,  j 1 ],
        [j 5  , j 3,  j 9 ]
      ]
  where
    n = Nothing
    j = Just


------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku = putStrLn . unlines . getStrArr

-- | Convert a Sudoku to a list of Strings
getStrArr :: Sudoku -> [String]
getStrArr s = map rowToString (rows s)

-- | Convert a Row into a String
rowToString :: Row -> String
rowToString = map convertMaybeIntToChar

-- | Converts a Maybe Int assumed to be in the range [1,9] to a Char
convertMaybeIntToChar :: Maybe Int -> Char
convertMaybeIntToChar Nothing   = '.'
convertMaybeIntToChar (Just i)  = head $ show i


-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku fp = do
  let val = validateFileName fp 

  if not val
  then error "readSudoku: Incorrect filepath"
  else do
    file <- readFile fp
    let sud = convertToSudoku $ lines file

    if isSudoku sud && isOkay sud
    then return sud
    else error "readSudoku: Invalid Sudoku found"

  where
    -- | Converts a .sud file to a Sudoku
    convertToSudoku :: [String] -> Sudoku
    convertToSudoku ls  = Sudoku (map (map charToCell) ls)

    -- | Converts a Char to a Cell
    charToCell :: Char -> Cell
    charToCell c
      | c == '.'  = Nothing
      | otherwise = Just (digitToInt c)

    -- | Ensures the filename ends with .sud
    validateFileName :: String -> Bool
    validateFileName s = validationHelper s == ".sud"

    -- | Retrives the last 4 chars in a String
    validationHelper :: String -> String
    validationHelper s 
      | length s > 4  = validationHelper $ tail s
      | length s == 4 = s
      | otherwise     = error "readSudoku: Incorrect filepath"


-- Check function with --- readSudoku "example.sud" >>= printSudoku


------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(1,rNums), (9,rNothing)] 

rNothing :: Gen Cell
rNothing = do return Nothing

rNums :: Gen Cell
rNums = do
  n <- choose (1,boardSize)
  return (Just n)

-- * C2
instance Arbitrary Sudoku where
  arbitrary = Sudoku <$> vectorOf 9 (vectorOf 9 cell)


-- * C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell

exampleBlock :: [Maybe Int]
exampleBlock = [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2];

exampleBadBlock :: [Maybe Int]
exampleBadBlock = [Just 7, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2];

-- * D1
-- | Checks whether duplicate digits exist in a block
isOkayBlock :: Block -> Bool
isOkayBlock b = length blockInts == length (nub blockInts)
  where blockInts = filter (/= Nothing) b


-- * D2
-- | Generates a list of all Blocks in a Sudoku
blocks :: Sudoku -> [Block]
blocks s = rs ++ getColumns rs ++ getBlocks rs
  where
    rs = rows s

-- | Gets the columns from a Sudoku's Rows
getColumns :: [Row] -> [Block]
getColumns = transpose

-- |  Divides a Sudoku's Rows into 3x9 segments
getBlocks :: [Row] -> [Block]
getBlocks [] = []
getBlocks rs = getGridColumns (take 3 rs) ++ getBlocks (drop 3 rs)

-- | Divides a 3x9 segment into 3x3 Blocks
getGridColumns :: [Row] -> [Block]
getGridColumns [] = []
getGridColumns rs = convertToBlock (take 3 cols) : getGridColumns (transpose (drop 3 cols))
  where
    cols = transpose rs

-- | Converts 3 3x1 Rows into one Block
convertToBlock :: [Row] -> Block
convertToBlock rs = concat $ transpose rs 

-- | Checks that the length of all Blocks in a Sudoku are 9
-- | and that the length of a Sudokus blocks is 3*9
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length bs == 3 * boardSize && all (\b -> length b == boardSize) bs
  where
    bs = blocks s

-- * D3

-- | Checks whether the blocks of a Sudoku are well formed
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1
exampleRow = head $ rows example

-- | Generates an array of where a Sudoku has empty Cells
blanks :: Sudoku -> [Pos]
blanks (Sudoku rs) = checkRow rs 0

-- | Iterate over Rows to find empty cells
checkRow :: [Row] -> Int -> [Pos]
checkRow [] _       = []
checkRow (r:rs) row = checkColumns r row 0 ++ checkRow rs (row + 1)

-- | Find empty Pos  in a Row
checkColumns :: Row -> Int -> Int -> [Pos]
checkColumns []     row col = []
checkColumns (x:xs) row col 
  | isNothing x  = (row,col) : checkColumns xs row (col + 1)
  | otherwise     = checkColumns xs row (col + 1)


-- | Generate an array of all feasible positions on a Sudoku board
allPos :: [Pos]
allPos = [(x,y) | x <- [0..(boardSize - 1)], y <- [0..(boardSize - 1)]]


-- | Test that the blanks of an blank Sudoku is equal 
-- | to allPos
prop_blanks_allBlanks ::  Bool
prop_blanks_allBlanks = blanks allBlankSudoku == allPos
-- prop_blanks_allBlanks = all (\p -> p `elem` allPos) (blanks allBlankSudoku) 


-- * E2

-- | Operator that updates the values of a list. 
(!!=) :: [a] -> (Int,a) -> [a]
[] !!= _ = []
(x:xs) !!= (i,y) 
  | i == 0    = y : xs
  | otherwise = x :  xs !!= (i-1,y)

-- | Checks that !!= doesn't change the length of the row and that the switch was successful.
prop_bangBangEquals_correct :: Row -> (Int, Cell) -> Bool
prop_bangBangEquals_correct row (i, y) = and [ 
    prop_bangBangEquals_length row (i,y), 
    prop_bangBangEquals_switched row (i,y) 
  ]
  where 
    -- | Checks that the change using !!= was successful.
    prop_bangBangEquals_switched :: Row -> (Int, Cell) -> Bool
    prop_bangBangEquals_switched row (i, y) | i < length row && i > 0 = ( row !!= (i,y) ) !! i == y
                                            | otherwise               = True

    -- | Checks that using "!!=" doesn't alter the length of the row.
    prop_bangBangEquals_length :: Row -> (Int, Cell) -> Bool
    prop_bangBangEquals_length row (i, y) = length ( row !!= (i,y) ) == length row



-- * E3

-- | Updates a Cell in a Sudoku
update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku rs) (row,col) c = Sudoku (rs !!= (row, (rs !! row) !!= (col,c) ) )


-- TO EXAMINER:
-- We have had a hard time generating random Pos. If you could explain
-- how to generate them so that the updated position does not need to 
-- be hard-coded that would be much appreciated! Something akin to the 
-- commented example below. We built a generator on row 386 but are 
-- unable to make this property use it.

-- | Test that update updates a provided Sudoku
prop_update_updated :: Sudoku  -> Maybe Int -> Bool
prop_update_updated s i = getValByPos (update s (0,0) i) (0,0) == i 
-- prop_update_updated s i (a,b) = getValByPos (update s (a,b) i) (a,b) == i 

getValByPos :: Sudoku -> Pos -> Maybe Int
getValByPos (Sudoku rs) (row,col) = (rs !! row) !! col 

rPos :: Gen Pos
rPos = do 
  a <- choose (0,boardSize-1)
  b <- choose (0,boardSize-1)
  return (a,b)

------------------------------------------------------------------------------
nums :: [Maybe Int]
nums = [Just n | n <- [1..boardSize]]

-- * F1

-- | Solves a given Sudoku
solve :: Sudoku -> Maybe Sudoku
solve s = listToMaybe (solve' s (blanks s))

-- | Checks that a provided Sudoku is valid, returns it
-- | if it is completed, and suggests new candidate 
-- | solutions if it's not.
solve':: Sudoku -> [Pos] -> [Sudoku]
solve' s bs 
  | not (isSudoku s) || not (isOkay s) = []
  | bs == []                           = [s]
  | otherwise                          = mapMaybe solve candidates
  where
    b = head bs
    candidates = [update s b n | n <- nums]
    
-- * F2
-- | Reads and solves a sudoku from a file
readAndSolve :: FilePath -> IO()
readAndSolve s = do
  sud <- readSudoku s
  let solved = solve sud
  case solved of 
    Nothing -> putStrLn "(no solution)"
    _       -> printSudoku $ fromJust solved


-- * F3
-- | Checks whether s1 is a possible solution to s2
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf s1 s2 = (isOkay s1 && isFilled s1) && solutionFor s1 s2 (nonBlanks s2)

-- | Iterates through an array of Pos to ensure that s1 and s2
-- | has the same value in each position
solutionFor :: Sudoku -> Sudoku -> [Pos] -> Bool
solutionFor _ _ []        = True
solutionFor s1 s2 (p:ps)  = getValByPos s1 p == getValByPos s2 p && solutionFor s1 s2 ps 

-- | Returns an array of all the Pos in a Sudoku
-- | that are currently filled by a value
nonBlanks :: Sudoku -> [Pos]
nonBlanks s = allPos \\ blanks s


-- * F4
-- | Tests the soundness of the solve function
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = property $ case solution of
  (Just sud) -> isSolutionOf sud s
  Nothing    -> True
  where 
    solution = solve s

-- | Utility method to run quickCheck with fewer tests
fewerChecks prop = quickCheckWith stdArgs{maxSuccess=30 } prop