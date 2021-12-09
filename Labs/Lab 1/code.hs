import Test.QuickCheck

-- val
val :: Integer
val = 35

-- functions
increment val = val + 1
decrement val = val - 1
fun a = a * val

-- properties (testing)
prop_nudge val = decrement (increment val) ~== val

m ~== n = abs(m - n) < epsilon
  where epsilon = 10e-14


abs' n | n < 0      = -n
       | otherwise  = n
-- abs' n = if n < 0 then -n else n