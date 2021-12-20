module Expr where

import Data.Maybe
import Parsing
import Test.QuickCheck
import Data.Char

data Expr = 
    Num Double
  | FunExpr Func Expr
  | BinExpr Op Expr Expr
  | X
  deriving (Eq,Show)

data Op = Add | Mul
  deriving (Eq,Show)

data Func = Sin | Cos
  deriving (Eq,Show)

-- | ------------------------------------------- |
-- | ----------------- Part 1A ----------------- |
-- | ------------------------------------------- |

-- | Constructor for a variable
x :: Expr
x  = X

-- | Converts a Double to an Expr
num :: Double -> Expr
num = Num

-- | Adds two :
add :: Expr -> Expr -> Expr
add = BinExpr Add

-- | Multiplies two Expr
mul :: Expr -> Expr -> Expr
mul = BinExpr Mul

-- | Applies the sin function to an Expr
sin :: Expr -> Expr
sin = FunExpr Sin

-- | Applies the cos function to an Expr
cos :: Expr -> Expr
cos = FunExpr Cos

-- | Calculate the number of operators and functions in an Expr
size :: Expr -> Int
size X                  = 0
size (Num _)            = 0
size (FunExpr _ e)     = 1 + size e
size (BinExpr _ e1 e2)  = 1 + size e1 + size e2

-- | ------------------------------------------- |
-- | ----------------- Part 1B ----------------- |
-- | ------------------------------------------- |
showFunc :: Func -> String
showFunc Sin = "sin"
showFunc Cos = "cos"

-- | Takes an expression and returns a string representation of it.
showExpr :: Expr -> String
showExpr X                  = "x"
showExpr (Num n)            = show n
showExpr (FunExpr f e)      = showFunc f ++ showExpr e 
showExpr (BinExpr Add e1 e2) = showExpr e1 ++ "+" ++ showExpr e2
showExpr (BinExpr Mul e1 e2) = showFactor e1 ++ "*" ++ showFactor e2


-- | Enable displaying of brackets for proper precendence of operations
showFactor :: Expr -> String
showFactor (BinExpr Add e1 e2) = "(" ++ showExpr (BinExpr Add e1 e2) ++ ")" 
showFactor e           = showExpr e


-- | ------------------------------------------- |
-- | ----------------- Part 1C ----------------- |
-- | ------------------------------------------- |

-- | Evaluate an expression to a Double value
eval :: Expr -> Double -> Double
eval e x = eval' e
  where
    eval' X                   = x 
    eval' (Num n)             = n
    eval' (BinExpr Add e1 e2) = eval' e1 + eval' e2
    eval' (BinExpr Mul e1 e2) = eval' e1 * eval' e2
    eval' (FunExpr Sin e)     = Prelude.sin (eval' e) 
    eval' (FunExpr Cos e)     = Prelude.cos (eval' e)

{-
-- | ------------------------------------------- |
-- | ----------------- Part 1D ----------------- |
-- | ------------------------------------------- |
-- | Read an Expr from a provided String 
readExpr :: String -> Maybe Expr
readExpr s = fst <$> parse expr (trim s)

-- | Remove all spaces in a string
trim :: String -> String
trim = filter ( not . isSpace )

-- | Parse functions for different levels of 
-- | expression complexity
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
    char 's' *> char 'i' *> char 'n'  
    char '('
    e <- expr
    char ')' 
    return $ Sin e
  <|> do
    char 's' *> char 'i' *> char 'n' *> (Sin <$> factor) 
  <|> do 
    char 'c' *> char 'o' *> char 's'  
    char '('
    e <- expr
    char ')'
    return $ Cos e
  <|> do
    char 'c' *> char 'o' *> char 's' *> (Cos <$> expr)
  <|> do
    char 'x'
    return X

-- | Parser for numbers in Strings
number :: Parser Double
number = read <$> oneOrMore digitDotOrMinus

-- | Parser for digits dots or minus signs
digitDotOrMinus :: Parser Char
digitDotOrMinus = sat isDigit <|> char '.' <|> char '-'


-- | Rewrites an expression to the associative form with all parenthesis to the right.
assoc :: Expr -> Expr
assoc (Add (Add e1 e2) e3)  = assoc (Add (assoc e1) (Add (assoc e2) (assoc e3)))  -- (1+2)+3 == 1+(2+3) 
assoc (Mul (Mul e1 e2) e3)  = Mul (assoc e1) (Mul (assoc e2) (assoc e3))
assoc (Add e1          e2)  = Add (assoc e1) (assoc e2)
assoc (Mul e1          e2)  = Mul (assoc e1) (assoc e2)
assoc (Num n)               = Num n
assoc (Sin e)               = Sin (assoc e)
assoc (Cos e)               = Cos (assoc e)
assoc X                     = X

-- | Runs assoc until further iterations no longer changes the input
-- | Note: Needed because there can exist expressions where one pass
-- | of assoc is not enough. See example below.
-- ((a + (b + c)) + (d + e)) --assoc--> (a + ((b + c) + (d + e))) --assoc--> (a + (b + (c + (d + e)))) --assoc--> (a + (b + (c + (d + e))))
assocs :: Expr -> Expr -> Expr
assocs e e'
  | e' == e   = e
  | otherwise = assocs e' (assoc e')

-- | ------------------------------------------- |
-- | ----------------- Part 1E ----------------- |
-- | ------------------------------------------- |

-- | Checks that running readExpr and showExpr consecutively on an expression doesn't change the expression.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = case readExpr $ showExpr e of
      Nothing -> False
      e'      -> assocs (fromJust e') (assoc $ fromJust e') == assocs e (assoc e)

-- | Generator for arbitrary Expr
arbExpr :: Int -> Gen Expr
arbExpr s = frequency [(1,rNum), (s,rBin s), (s,rTrig s), (s, rX)]
  -- s protects from building infinite trees by decreasing 
  -- the chance of picking a binary expression
  where 
    rNum = do
      n <- elements [0..99]
      return (Num n)

    rX = do
      return X

    rTrig s = do
      let s' = s `div` 2
      op <- elements [Sin, Cos]
      e <- arbExpr s'
      return $ op e

    rBin s = do
      let s' = s `div` 2
      op <- elements [Mul,Add]
      e1 <- arbExpr s'
      e2 <- arbExpr s'
      return $ op e1 e2

-- Enable in QuickCheck
instance Arbitrary Expr where
  arbitrary = sized arbExpr

-- | ------------------------------------------- |
-- | ----------------- Part 1F ----------------- |
-- | ------------------------------------------- |

-- | Simplifies an expression down to its smallest form.
simplify :: Expr -> Expr
simplify e
  |  e' == e   = e
  | otherwise = simplify e'
  where 
    e' = simplify' e 

-- | Simplifies an expression one step.
simplify' :: Expr -> Expr
simplify' X                        = X
simplify' (Num n)                  = Num n
simplify' (Sin (Num 0))            = Num 0
simplify' (Sin e)                  = Sin (simplify' e)
simplify' (Cos (Num 0))            = Num 1
simplify' (Cos e)                  = Cos (simplify' e)

simplify' (Mul (Num 0) _)          = Num 0
simplify' (Mul _ (Num 0))          = Num 0
simplify' (Mul (Num 1) e)          = simplify' e
simplify' (Mul e (Num 1))          = simplify' e
simplify' (Mul (Num n1) (Num n2))  = Num (n1*n2)
simplify' (Mul e1 e2)              = Mul (simplify' e1) (simplify' e2)

simplify' (Add (Num 0) e)                 = simplify' e
simplify' (Add e (Num 0))                 = simplify' e

simplify' (Add (Num n1) (Num n2))         = Num (n1+n2)
simplify' (Add e1 e2)                     = Add (simplify' e1) (simplify' e2)

-- | Additional simplifications that make some expressions nicer
-- | and allows further simplification
simplify' (Add (Add X (Num n1)) (Num n2)) = Add X (Num (n1+n2))  -- (X + n1) + n2 = X + (n1 + n2)
simplify' (Add (Add (Num n1) X) (Num n2)) = Add X (Num (n1+n2))  -- (n1 + X) + n2 = X + (n1 + n2)
simplify' (Add X (Add (Num n) X))         = Add (Num n) (Add X X) -- X + (n + X) = n + (X + X)
simplify' (Add X (Add X (Num n)))         = Add (Num n) (Add X X) -- X + (X + n) = n + (X + X)
simplify' (Add (Add (Num n) X) X)         = Add (Num n) (Add X X) -- (n + X) + X = n + (X + X)
simplify' (Add (Add X (Num n)) X)         = Add (Num n) (Add X X) -- (X + n) + X = n + (X + X)


-- | Checks that the value of the expression and the simplified expression are equal.
prop_SameValue :: Expr -> Double -> Bool 
prop_SameValue e d = eval e d == eval (simplify e) d

-- | Checks that a simplified expression is of the simplest form
prop_SimplestForm :: Expr -> Bool
prop_SimplestForm e = formatChecker $ simplify e

-- | Checks that an expression is of an acceptable minimal form
formatChecker :: Expr -> Bool
formatChecker X                        = True
formatChecker (Num n)                  = True
formatChecker (Sin (Num 0))            = False
formatChecker (Sin e)                  = formatChecker e
formatChecker (Cos (Num 0))            = False
formatChecker (Cos e)                  = formatChecker e

formatChecker (Mul (Num 0) _)          = False
formatChecker (Mul _ (Num 0))          = False
formatChecker (Mul (Num 1) e)          = False
formatChecker (Mul e (Num 1))          = False
formatChecker (Mul (Num n1) (Num n2))  = False
formatChecker (Mul e1 e2)              = formatChecker e1 && formatChecker e2

formatChecker (Add (Num 0) e)          = formatChecker e
formatChecker (Add e (Num 0))          = formatChecker e
formatChecker (Add (Num n1) (Num n2))  = False
formatChecker (Add e1 e2)              = formatChecker e1 && formatChecker e2


-- | ------------------------------------------- |
-- | ----------------- Part 1G ----------------- |
-- | ------------------------------------------- |

-- | Differentiates an expression and returns the simplified form.
differentiate :: Expr -> Expr
differentiate = simplify . differentiate'

-- | Differentiate expressions
differentiate' :: Expr -> Expr
-- differentiate' (Add e1 e2) = Add (differentiate' e1) (differentiate' e2)
-- differentiate' (Mul e1 e2) = Add (Mul (differentiate' e1) e2) (Mul e1 (differentiate' e2))
differentiate' X           = Num 1
-- differentiate' (Sin e)     = Mul (differentiate' e) (Cos e)
-- differentiate' (Cos e)     = Mul (differentiate' e) (Mul (Num (-1)) (Sin e))
differentiate' _           = Num 0 
-}