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
showExpr X                                = "x"
showExpr (Num n)                          = show n
showExpr (FunExpr f e)                    = showFunc f ++ showTrig e 
showExpr (BinExpr Add e1 e2)              = showExpr e1 ++ "+" ++ showExpr e2
showExpr (BinExpr Mul e1 e2)              = showFactor e1 ++ "*" ++ showFactor e2


-- | Enable displaying of brackets for proper precendence of operations
showFactor :: Expr -> String
showFactor (BinExpr Add e1 e2)  = "(" ++ showExpr (BinExpr Add e1 e2) ++ ")" 
showFactor e                    = showExpr e

showTrig :: Expr -> String
showTrig (BinExpr op e1 e2) = "(" ++ showExpr (BinExpr op e1 e2) ++ ")" 
showTrig e                  = showExpr e

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
  return $ foldl (BinExpr Add) t ts

term = do
  t <- factor
  ts <- zeroOrMore (do char '*'; factor)
  return $ foldl (BinExpr Mul) t ts

factor = Num <$> readsP
  <|> do 
    char '(' 
    e <- expr  
    char ')'
    return e
  <|> do
    char 's' *> char 'i' *> char 'n' *> (FunExpr Sin <$> factor) 
  <|> do
    char 'c' *> char 'o' *> char 's' *> (FunExpr Cos <$> factor)
  <|> do
    char 'x'
    return X



-- | Rewrites an expression to the associative form with all parenthesis to the right.
assoc :: Expr -> Expr
assoc X                                    = X
assoc (Num n)                              = Num n
assoc (FunExpr f e)                        = FunExpr f (assoc e)
assoc (BinExpr Add (BinExpr Add e1 e2) e3) = assoc (BinExpr Add (assoc e1) (BinExpr Add (assoc e2) (assoc e3)))  -- (1+2)+3 == 1+(2+3) 
assoc (BinExpr Mul (BinExpr Mul e1 e2) e3) = assoc (BinExpr Mul (assoc e1) (BinExpr Mul (assoc e2) (assoc e3)))
assoc (BinExpr op e1 e2)                   = BinExpr op (assoc e1) (assoc e2)


-- | ------------------------------------------- |
-- | ----------------- Part 1E ----------------- |
-- | ------------------------------------------- |

-- | Checks that running readExpr and showExpr consecutively on an expression doesn't change the expression.
prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr e = case readExpr $ showExpr e of
      Nothing -> False
      e'      -> assoc (fromJust e') == assoc e 

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
      return $ FunExpr op e

    rBin s = do
      let s' = s `div` 2
      op <- elements [Mul,Add]
      e1 <- arbExpr s'
      e2 <- arbExpr s'
      return $ BinExpr op e1 e2

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
simplify' X                               = X
simplify' (Num n)                         = Num n
simplify' (FunExpr Sin (Num n))           = Num $ Prelude.sin n
simplify' (FunExpr Sin e)                 = FunExpr Sin (simplify' e)
simplify' (FunExpr Cos (Num n))           = Num $ Prelude.cos n
simplify' (FunExpr Cos e)                 = FunExpr Cos (simplify' e)

simplify' (BinExpr Mul (Num 0) _)         = Num 0
simplify' (BinExpr Mul _ (Num 0))         = Num 0
simplify' (BinExpr Mul (Num 1) e)         = simplify' e
simplify' (BinExpr Mul e (Num 1))         = simplify' e
simplify' (BinExpr Mul (Num n1) (Num n2)) = Num (n1*n2)
simplify' (BinExpr Mul e1 e2)             = BinExpr Mul (simplify' e1) (simplify' e2)

simplify' (BinExpr Add (Num 0) e)         = simplify' e
simplify' (BinExpr Add e (Num 0))         = simplify' e
simplify' (BinExpr Add (Num n1) (Num n2)) = Num (n1+n2)

-- | Additional simplifications that make some expressions nicer
-- | and allows further simplification
simplify' (BinExpr Add (BinExpr Add X (Num n1)) (Num n2)) = BinExpr Add X (Num (n1+n2))           -- (X + n1) + n2 = X + (n1 + n2)
simplify' (BinExpr Add (BinExpr Add (Num n1) X) (Num n2)) = BinExpr Add X (Num (n1+n2))           -- (n1 + X) + n2 = X + (n1 + n2)
simplify' (BinExpr Add X (BinExpr Add (Num n) X))         = BinExpr Add (Num n) (BinExpr Add X X) -- X + (n + X) = n + (X + X)
simplify' (BinExpr Add X (BinExpr Add X (Num n)))         = BinExpr Add (Num n) (BinExpr Add X X) -- X + (X + n) = n + (X + X)
simplify' (BinExpr Add (BinExpr Add (Num n) X) X)         = BinExpr Add (Num n) (BinExpr Add X X) -- (n + X) + X = n + (X + X)
simplify' (BinExpr Add (BinExpr Add X (Num n)) X)         = BinExpr Add (Num n) (BinExpr Add X X) -- (X + n) + X = n + (X + X)

simplify' (BinExpr Add e1 e2)                             = BinExpr Add (simplify' e1) (simplify' e2)


-- | Checks that the value of the expression and the simplified expression are equal.
prop_SameValue :: Expr -> Double -> Bool 
prop_SameValue e d = eval e d - eval (simplify e) d <= 0.00001

-- | Checks that a simplified expression is of the simplest form
prop_SimplestForm :: Expr -> Bool
prop_SimplestForm e = formatChecker $ simplify e

-- | Checks that an expression is of an acceptable minimal form

formatChecker :: Expr -> Bool
formatChecker X                                = True
formatChecker (Num n)                          = True
formatChecker (FunExpr Sin (Num 0))            = False
formatChecker (FunExpr Sin e)                  = formatChecker e
formatChecker (FunExpr Cos (Num 0))            = False
formatChecker (FunExpr Cos e)                  = formatChecker e

formatChecker (BinExpr Mul (Num 0) _)          = False
formatChecker (BinExpr Mul _ (Num 0))          = False
formatChecker (BinExpr Mul (Num 1) e)          = False
formatChecker (BinExpr Mul e (Num 1))          = False
formatChecker (BinExpr Mul (Num n1) (Num n2))  = False
formatChecker (BinExpr Mul e1 e2)              = formatChecker e1 && formatChecker e2

formatChecker (BinExpr Add (Num 0) e)          = formatChecker e
formatChecker (BinExpr Add e (Num 0))          = formatChecker e
formatChecker (BinExpr Add (Num n1) (Num n2))  = False
formatChecker (BinExpr Add e1 e2)              = formatChecker e1 && formatChecker e2


-- | ------------------------------------------- |
-- | ----------------- Part 1G ----------------- |
-- | ------------------------------------------- |

-- | Differentiates an expression and returns the simplified form.
differentiate :: Expr -> Expr
differentiate = simplify . differentiate'

-- | Differentiate expressions
differentiate' :: Expr -> Expr
differentiate' (BinExpr Add e1 e2) = BinExpr Add (differentiate' e1) (differentiate' e2)
differentiate' (BinExpr Mul e1 e2) = BinExpr Add (BinExpr Mul (differentiate' e1) e2) (BinExpr Mul e1 (differentiate' e2))
differentiate' X                   = Num 1
differentiate' (FunExpr Sin e)     = BinExpr Mul (differentiate' e) (FunExpr Cos e)
differentiate' (FunExpr Cos e)     = BinExpr Mul (differentiate' e) (BinExpr Mul (Num (-1)) (FunExpr Sin e))
differentiate' _                   = Num 0 

