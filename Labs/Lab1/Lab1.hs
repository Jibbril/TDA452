import Test.QuickCheck
{- Lab 1
   Date: 21-11-02
   Authors: Jibbril Ndaw Berbres and Valdemar Stenhammar
   Lab group: 32
 -}
--------------------------------------------
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A ------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower _ k = k + 1


-- B -------------------------
power1 :: Integer -> Integer -> Integer
-- Two implementations 
-- power1 n 0 = 1
-- power1 n k = product [n | _ <- [1..k]]

power1 n k | k == 0     = 1
           | otherwise  = product [n | _ <- [1..k]]


-- C -------------------------
-- power2
power2 :: Integer -> Integer -> Integer
power2 n k | k == 0     = 1
           | even k     = power2 (n*n) (div k 2)
           | odd k      = n * power2 n (k-1)


-- D -------------------------
{- 

<Describe your test cases here>
  power 2 4 Even exponent 16
  power 2 5 Odd exponent, should return 32
  power 2 0 Zero exponent, should return 1
  power 0 2 Zero base, should return 0
 -}

-- 
nums = [(2,4), (2,5), (2,0), (0,2)]
prop_powers n k = and [((power n k) == (power1 n k)), ((power1 n k) == (power2 n k))]

--
powerTest :: Bool
powerTest = and [ prop_powers n k | (n,k) <- nums]

--
prop_powers' n k = prop_powers (abs n)  (abs k)