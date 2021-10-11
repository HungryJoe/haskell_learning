-- 31
testIsPrime :: Bool
testIsPrime = isPrime 7 && not (isPrime 6) && isPrime 37 && isPrime 2 && not (isPrime 26)

isPrime :: Integral a => a -> Bool
isPrime n = foldl isDivisible True [2..(round $ sqrt $ fromIntegral n)]
    where isDivisible acc i = n `mod` i /= 0 && acc  -- Want True unless i divides n for any i
