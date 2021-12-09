import Test.QuickCheck
import Data.Maybe(fromJust)

data Expr = 
    Num Integer 
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
  deriving Eq

instance Show Expr where
  show = showExpr

ex1 = Mul (Add (Var "y") (Num 2)) (Var "x")
ex2 = Add (Var "x") (Mul (Num 2) (Var "y"))

vars :: Expr -> [String]
vars (Num n)      = []
vars (Var s)      = [s]
vars (Add e1 e2)  = vars e1 ++ vars e2
vars (Mul e1 e2)  = vars e1 ++ vars e2

-- key/value list of values associated with variables
type Table = [(String,Integer)]

-- Used to generate tables
newtype Env = Env Table
  deriving Show

instance Arbitrary Env where
  arbitrary = do
    (l,m,n) <- arbitrary
    return $ Env [("x",l), ("y",m), ("z",n)]
  

eval :: Table -> Expr -> Integer
eval t e = eval' e
  where
    eval' (Num n)     = n
    eval' (Add e1 e2) = eval' e1 + eval' e2
    eval' (Mul e1 e2) = eval' e1 * eval' e2
    eval' (Var x)     = fromJust $ lookup x t -- Could also use look k t

    look k [] = error "No value for " ++ k
    look k ((k',v):t)
      | k == k'   = v
      | otherwise = look k t


showExpr :: Expr -> String
showExpr (Var x)      = x
showExpr (Num n)      = show n
showExpr (Add e1 e2)  = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2)  = showFactor e1 ++ " * " ++ showFactor e2

showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")" 
showFactor e           = showExpr e

-- Generators for expressions
range = 4
level = fromInteger range -- converts to int


rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (s,rBin s), (s,rVar)]
  -- s protects from building infinite trees by decreasing 
  -- the chance of picking a binary expression
  where 
    rVar = elements $ map Var ["x", "y", "z"]
    rNum = do
      n <- elements [-range..range]
      return (Num n)
    -- rNum = elements $ map Num [-range..range] --- equivalent

    rBin s = do
      let s' = (s `div` 2)
      op <- elements [Mul,Add]
      e1 <- rExpr s'
      e2 <- rExpr s'
      return $ op e1 e2

-- Enable in QuickCheck
instance Arbitrary Expr where
  arbitrary = sized rExpr

-- Differentiate expressions
differentiate :: String -> Expr -> Expr
-- differentiate x e
-- Take the derivative of e with respect to x
differentiate x (Add e1 e2)       = mul (differentiate x e1) (differentiate x e2)
differentiate x (Mul e1 e2)       = add (mul (differentiate x e1) e2) (mul e1 (differentiate x e2))
differentiate x (Var y) | x == y  = Num 1
differentiate _ _                 = Num 0 

-- Smart operations that simplify Expr:s
add (Num n) (Num m) = Num (n + m)
add (Num 0) e       = e
add e (Num 0)       = e
add e1 e2           = Add e1 e2

mul (Num n) (Num m) = Num (n + m)
mul (Num 0) e       = Num 0
mul e (Num 0)       = Num 0
mul (Num 1) e       = e
mul e (Num 1)       = e
mul e1 e2           = Mul e1 e2


