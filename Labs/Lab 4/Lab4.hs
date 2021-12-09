import Data.Maybe

data Expr = 
    Num Double
  | Cos Expr 
  | Sin Expr
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
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
x = Cos (Add (Mul (Num 3) (Num 2)) (Var "x")) -- What's the meaning of this?
x2 = Cos (Mul (Add (Num 3) (Num 2)) (Var "y")) -- What's the meaning of this?
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
size (Num _)      = 0
size (Var a)      = 0
size (Cos e)      = 1 + size e
size (Sin e)      = 1 + size e
size (Add e1 e2)  = 1 + size e1 + size e2
size (Mul e1 e2)  = 1 + size e1 + size e2

-- | ------------------------------------------- |
-- | ----------------- Part 1B ----------------- |
-- | ------------------------------------------- |
showExpr :: Expr -> String
showExpr (Var x)      = x
showExpr (Num n)      = show n
showExpr (Sin e)      = "sin(" ++ showExpr e ++ ")"
showExpr (Cos e)      = "cos(" ++ showExpr e ++ ")"
showExpr (Add e1 e2)  = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2)  = showFactor e1 ++ " * " ++ showFactor e2

showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")" 
showFactor e           = showExpr e

eval :: Table -> Expr -> Double
eval t e = eval' e
  where
    eval' (Num n)     = n
    eval' (Add e1 e2) = eval' e1 + eval' e2
    eval' (Mul e1 e2) = eval' e1 * eval' e2
    -- eval' (Sin e1 e2) = 
    -- eval' (Cos e1 e2) =
    eval' (Var x)     = fromJust $ lookup x t -- Could also use look k t

    look k [] = error "No value for " ++ k
    look k ((k',v):t)
      | k == k'   = v
      | otherwise = look k t