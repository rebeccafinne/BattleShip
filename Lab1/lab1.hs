import Test.QuickCheck

power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

--Part 1
--k+1 steps

--Part 2
power1 :: Integer -> Integer -> Integer
power1 n k  | k < 0 = error "power1: negative argument"
power1 n 0 = 1
power1 n k = product( replicate (fromInteger k) (fromInteger n))

--Part 3
power2 :: Integer -> Integer -> Integer
power2 n k  | k < 0 = error "power2: negative argument"
power2 n 0 = 1
power2 n k | even k = power2 (n*n) (k `div` 2)
power2 n k | odd k = n * power2 n (k-1)

--Part 4
--A
--n=0 and k=0
--positive integers for n and k

--B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) == (power n k == power2 n k)

--C
test :: Bool
test = and [(prop_powers) n k  | n <- [(-10)..10], k <- [0..10]]

--D
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = (power n k' == power1 n k') == (power n k' == power2 n k')
    where k' = abs k













