import Data.Maybe
import Parsing

data Expr = 
    Num Double
  | Cos Expr 
  | Sin Expr
  | Add Expr Expr
  | Mul Expr Expr
  | X
  deriving (Eq,Show)

type Table = [(String,Double)]

-- Approximate plan:
-- table = [("x", 5),("y", 7)]
-- calculate (2*x + 3*y) table
-- parse   -> Add (Mul 2 x) (Mul 3 y)
-- lookup  -> Add (Mul 2 5) (Mul 3 7)
-- eval    -> Add 10 21 
-- show eval -> 31

-- | ------------------------------------------- |
-- | ----------------- Part 1A ----------------- |
-- | ------------------------------------------- |
x :: Expr
x  = Cos (Add (Mul (Num 3) (Num 2)) X)
x2 = Cos (Mul (Add (Num 3) (Num 2)) X)

-- | Converts a Double to an Expr
num :: Double -> Expr
num n = Num n

-- | Adds two Expr
add :: Expr -> Expr -> Expr
add e1 e2 = Add e1 e2

-- | Multiplies two Expr
mul :: Expr -> Expr -> Expr
mul e1 e2 = Mul e1 e2

-- | Applies the sin function to an Expr
sin :: Expr -> Expr
sin e = Sin e

-- | Applies the cos function to an Expr
cos :: Expr -> Expr
cos e = Cos e

-- | Calculate the number of operators and functions
-- | in an Expr
size :: Expr -> Int
size X            = 0
size (Num _)      = 0
size (Cos e)      = 1 + size e
size (Sin e)      = 1 + size e
size (Add e1 e2)  = 1 + size e1 + size e2
size (Mul e1 e2)  = 1 + size e1 + size e2

-- | ------------------------------------------- |
-- | ----------------- Part 1B ----------------- |
-- | ------------------------------------------- |

-- | Takes an expression and returns a string a string representation of it.
showExpr :: Expr -> String
showExpr X            = "x"
showExpr (Num n)      = show n
showExpr (Sin e)      = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e)      = "cos(" ++ showExpr e ++ ")"
showExpr (Add e1 e2)  = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2)  = showFactor e1 ++ "*" ++ showFactor e2

-- | 
showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")" 
showFactor e           = showExpr e


-- | ------------------------------------------- |
-- | ----------------- Part 1C ----------------- |
-- | ------------------------------------------- |

eval :: Expr -> Double -> Double
eval e x = eval' e
  where
    eval' (X)         = x 
    eval' (Num n)     = n
    eval' (Add e1 e2) = eval' e1 + eval' e2
    eval' (Mul e1 e2) = eval' e1 * eval' e2
    eval' (Sin e) = Prelude.sin (eval' e) 
    eval' (Cos e) = Prelude.cos (eval' e)


-- | ------------------------------------------- |
-- | ----------------- Part 1D ----------------- |
-- | ------------------------------------------- |

readExpr :: String -> Maybe Expr
readExpr s = fst <$> parse expr (trim s)

trim :: String -> String
trim s = filter (/= ' ') s

expr, term, factor :: Parser Expr
expr = do 
  t <- term
  ts <- zeroOrMore (do char '+'; term)
  return $ foldl Add t ts

term = do
  t <- factor
  ts <- zeroOrMore (do char '*'; factor)
  return $ foldl Mul t ts

factor = Num <$> number
  <|> do 
    char '('
    e <- expr
    char ')'
    return e
  <|> do
    char 's' -- char "sin(3)" -> ("s", "in(3)")
    char 'i'
    char 'n'
    char '('
    e <- expr
    char ')'
    return $ Sin e
  <|> do
    char 'c'
    char 'o'
    char 's'
    char '('
    e <- expr
    char ')'
    return $ Cos e
  <|> do
    char 'x'
    return X

number :: Parser Double
number = read <$> oneOrMore digit
