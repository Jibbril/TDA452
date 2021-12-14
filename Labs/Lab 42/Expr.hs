module Expr where

import Data.Maybe
import Parsing
import Test.QuickCheck
import Data.Char

data Expr = 
    Num Double
  | Cos Expr 
  | Sin Expr
  | Add Expr Expr
  | Mul Expr Expr
  | X
  deriving (Eq,Show)

type Table = [(String,Double)]

x,x2,x3,x4 :: Expr
x  = Cos (Add (Mul (Num 3) (Num 2)) X)
x2 = Cos (Mul (Add (Num 3) (Num 2)) X)
x3 = Sin (Add (Sin (Num 4.0)) (Mul (Add (Num 3.0) (Num 0.0)) (Num 2.0)))
x4 = Mul (Add (Add (Mul (Add (Num 0.0) (Num 1.0)) X) (Sin (Cos (Num (-1.0))))) X) (Sin (Cos (Mul (Add (Num 0.0) (Num 0.0)) (Cos (Num 4.0)))))

-- | ------------------------------------------- |
-- | ----------------- Part 1A ----------------- |
-- | ------------------------------------------- |

-- | Converts a Double to an Expr
num :: Double -> Expr
num = Num

-- | Adds two Expr
add :: Expr -> Expr -> Expr
add = Add

-- | Multiplies two Expr
mul :: Expr -> Expr -> Expr
mul = Mul

-- | Applies the sin function to an Expr
sin :: Expr -> Expr
sin = Sin

-- | Applies the cos function to an Expr
cos :: Expr -> Expr
cos = Cos

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
showExpr X             = "x"
showExpr (Num n)       = show n
showExpr (Sin e)       = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e)       = "cos(" ++ showExpr e ++ ")"
showExpr (Add e1 e2)   = showExpr e1 ++ "+" ++ showExpr e2
showExpr (Mul e1 e2)   = showFactor e1 ++ "*" ++ showFactor e2

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
    eval' X         = x 
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
trim = filter ( not . isSpace )

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

-- | TODO: Fix this using reads in stead of digitDotOrMinus
number :: Parser Double
number = read <$> oneOrMore digitDotOrMinus

digitDotOrMinus :: Parser Char
digitDotOrMinus = sat isDigit <|> char '.' <|> char '-'


-- | Runs assoc until further iterations no longer changes the input
-- | Note: Needed because there can exist expressions where one pass
-- | of assoc is not enough. See example below.
-- ((a + (b + c)) + (d + e)) --assoc--> (a + ((b + c) + (d + e))) --assoc--> (a + (b + (c + (d + e)))) --assoc--> (a + (b + (c + (d + e))))
assocs :: Expr -> Expr -> Expr
assocs e e'
  | e' == e   = e
  | otherwise = assocs e' (assoc e')

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


-- | ------------------------------------------- |
-- | ----------------- Part 1E ----------------- |
-- | ------------------------------------------- |

-- | Checks that running readExpr and showExpr consecutively on an expression doesn't change the expression.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = case (readExpr $ showExpr e) of
      Nothing -> False
      e'      -> assocs (fromJust e') (assoc $ fromJust e') == assocs e (assoc e)

arbExpr :: Int -> Gen Expr
arbExpr = rExpr 


rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (s,rBin s), (s,rTrig s), (s, rX)]
  -- s protects from building infinite trees by decreasing 
  -- the chance of picking a binary expression
  where 
    range = 4

    rNum = do
      n <- elements [0..range]
      return (Num n)
    -- rNum = elements $ map Num [-range..range] --- equivalent

    rX = do
      return X

    rTrig s = do
      let s' = (s `div` 2)
      op <- elements [Sin, Cos]
      e <- rExpr s'
      return $ op e

    rBin s = do
      let s' = (s `div` 2)
      op <- elements [Mul,Add]
      e1 <- rExpr s'
      e2 <- rExpr s'
      return $ op e1 e2

-- Enable in QuickCheck
instance Arbitrary Expr where
  arbitrary = sized rExpr

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

simplify' (Add (Num 0) e)          = simplify' e
simplify' (Add e (Num 0))          = simplify' e
simplify' (Add (Num n1) (Num n2))  = Num (n1+n2)
simplify' (Add e1 e2)              = Add (simplify' e1) (simplify' e2)

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
differentiate' (Add e1 e2) = Mul (differentiate' e1) (differentiate' e2)
differentiate' (Mul e1 e2) = Add (Mul (differentiate' e1) e2) (Mul e1 (differentiate' e2))
differentiate' X           = Num 1
differentiate' (Sin e)     = Mul (differentiate' e) (Cos e)
differentiate' (Cos e)     = Mul (differentiate' e) (Mul (Num (-1)) (Sin e))
differentiate' _           = Num 0 



