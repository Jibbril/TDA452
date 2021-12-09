import Test.QuickCheck

-- Recursive datatypes
data Expr = 
    Num Integer 
  | Add Expr Expr
  | Mul Expr Expr
  deriving Eq

instance Show Expr where
  show = showExpr

ex1 = Mul (Add (Num 1) (Num 2)) (Num 4) 
ex2 = Add (Num 1) (Mul (Num 2) (Num 4))

eval :: Expr -> Integer
eval (Num n)      = n
eval (Add e1 e2)  = eval e1 + eval e2
eval (Mul e1 e2)  = eval e1 * eval e2

showExpr :: Expr -> String
showExpr (Num n)      = show n
showExpr (Add e1 e2)  = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Mul e1 e2)  = showFactor e1 ++ " * " ++ showFactor e2

showFactor :: Expr -> String
showFactor (Add e1 e2) = "(" ++ showExpr (Add e1 e2) ++ ")" 
showFactor e           = showExpr e

-- Generate random expressions
range = 4
level = fromInteger range -- converts to int


rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum), (s,rBin s)]
  -- s protects from building infinite trees by decreasing 
  -- the chance of picking a binary expression
  where 
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

-- Main game
main :: IO()
main = do 
  -- e <- rExpr level
  es <- sample' $ rExpr level
  let e = es !! level
  putStrLn $ "What is the value of " ++ show e
  ans <- getLine
  let v = show $ eval e
  if (ans == v)
  then putStrLn "Correct\n"
  else putStrLn $ "Fail! Correct answer was: " ++ v ++ "\n"
  main


-- 