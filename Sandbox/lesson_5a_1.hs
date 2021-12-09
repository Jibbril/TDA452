import Test.QuickCheck
import Data.Char

data Expr = Num Int | Add Expr Expr | Mul Expr Expr


type ParserFun a = String -> Maybe (a,String)

num :: ParserFun Integer
num s = case span isDigit s of
  (d:ds, rest) -> Just (read (d:ds),rest)
  _            -> Nothing

addition0 :: ParserFun Integer
addition0 s = case num s of 
  Just (n,'+':r)  -> case num r of
    Just (m, r')    -> Just (n+m,r')
    _               -> Nothing
  _               -> Nothing


multiplication0 :: ParserFun Integer
multiplication0 s = case num s of 
  Just (n,'*':r)  -> case num r of
    Just (m, r')    -> Just (n*m,r')
    _               -> Nothing
  _               -> Nothing

calculation0 s = case addition0 s of 
  Nothing -> multiplication0 s
  ok      -> ok