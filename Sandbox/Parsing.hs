-- module Parsing 
-- ( Parser,
--   parse,
--   failure,sat,item,char,digit,
--   (+++),
--   oneOrMore,zeroOrMore,chain
-- )

-- where

import Data.Char
import Data.Maybe

import Control.Applicative  (Applicative(..))
import Control.Monad        (liftM,ap)


u = undefined -- For simplicity

-- Abstract data type representing a Parser
data Parser a = P (String -> Maybe (a,String))

-- Runs the Parser on a given argument
parse :: Parser a -> String -> Maybe (a,String)
parse (P f) s = f s

-- Parser that can never succeed
failure :: Parser a
failure = P (\s -> Nothing)

-- Parser that always succeeds
success :: a -> Parser a
success a = P (\s -> Just (a,s))

-- Parse a single character
item :: Parser Char
item = P $ \s -> 
  case s of 
    (c:s')  -> Just (c,s')
    ""      -> Nothing

infixr 5 +++
-- Try the first parser, if it fails, try the second
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P $ \s -> case parse p s of
  Nothing -> parse q s
  ok      -> ok


infixl 1 >*>
-- Parse using one Parser and then use the results
-- to parse using another Parser
(>*>) :: Parser a -> (a -> Parser b) -> Parser b
p >*> f = P $ \s -> 
  case parse p s of
    Just (a,s') -> parse (f a) s'
    _           -> Nothing

-- Parse a single thing satisfying p
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item 
  case p c of 
    True -> success c
    False -> failure
-- equivalent
-- sat p = item >*> \c -> case p c of
--   True -> success c
--   False -> failure


-- Parse a digit character
digit :: Parser Char
digit = sat isDigit

-- Parse a specific character
char c = sat (== c)


-- Parse any lowercase letter followed by 
-- its uppercase equivalent (aA, bB etc)
ex1 = do
  c <- sat isAsciiLower
  char (toUpper c)
-- ex1 = sat isAsciiLower >*> \c -> char (toUpper c)
-- ex1 = sat iisAsciiLower >*> char . toUpper


-- Parse zero or more things
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p +++ success []


-- Parse one or more things
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do
  a <- p
  as <- zeroOrMore p
  return (a:as)


-- Parse a list of as, separated by bs
chain :: Parser a -> Parser b -> Parser [a]
chain p q = do
  a <- p
  as <- zeroOrMore (q >> p)
  -- as <- zeroOrMore (q >*> \_ -> p)
  return (a:as)
-- Command:  parse (chain digit (char ',')) "1,2,3,4jfskdlf"



instance Functor Parser where
  fmap = liftM
instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  (>>=) = (>*>) -- usually called "bind"
  return = success

