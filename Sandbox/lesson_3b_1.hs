-- classes
class Num a where -- Works somewhat like interfaces in java
  (+), (*), (-) :: a -> a -> a
  negate, abs, signum :: a -> a
  fromInteger :: Integer -> a

instance Num Int where -- Provide instructions for operators/functions
instance Num Double where --


class Eq a where 
  (===), (/==) :: a -> a -> Bool

  a /= b = not (a == b) -- default implementation
  a == b = not (a /= b) -- default implementation

instance Eq Int where -- ...


-- Custom classes
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red     == Red    = True
  Yellow  == Yellow = True
  Green   == Green  = True
  _       == _      = False


-- Equality for instance pairs
instance (Eq a, Eq b) => Eq (a,b) where
  (x1,y1) == (x2,y2) = x1 == x2 && y1 == y2


