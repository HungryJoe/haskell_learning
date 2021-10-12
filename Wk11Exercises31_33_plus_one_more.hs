-- 31
testIsPrime :: Bool
testIsPrime = isPrime 7 && not (isPrime 6) && isPrime 37 && isPrime 2 && not (isPrime 26)

isPrime :: Integral a => a -> Bool
isPrime n = foldl isDivisible True [2..(round $ sqrt $ fromIntegral n)]
    where isDivisible acc i = n `mod` i /= 0 && acc  -- Want True unless i divides n for any i

-- Reddit problem
-- Sum of a list of numbers times their (1-based) indices
-- WITHOUT VARIABLES
testSumOfIndexProducts :: Bool
testSumOfIndexProducts = sumOfIndexProducts [1,2,3] == 14 && sumOfIndexProducts [5,4,2] == 19

sumOfIndexProducts :: [Int] -> Int
sumOfIndexProducts = sum . zipWith (*) [1..]

-- 32
testGcd' :: Bool
testGcd' = [gcd' 36 63, gcd' 3 6, gcd' 1071 462] == [9, 3, 21]

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b
    | a < b = gcd' b a
    | otherwise = gcd' b $ a `mod` b

-- 33
testAreCoprime :: Bool
testAreCoprime = areCoprime 5 3 && areCoprime 35 64 && not (areCoprime 44 55) && not (areCoprime 100 50)

areCoprime :: Int -> Int -> Bool
areCoprime a = (1 ==) . gcd' a
